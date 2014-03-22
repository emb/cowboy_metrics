%% Copyright (c) 2013-2015, Ed Mohamed Barwani <ed.barwani@gmail.com>
%% All rights reserved.

%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are
%% met:

%% 1. Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.

%% 2. Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.

%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%% HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @doc Application Entry Point.
%% Allow cowboy web servers to start gathering SNMP metrics. This
%% module provide mechanism to either generate appropriate
%% `cowboy:onrequest_fun/1' and `cowboy:onrequest_fun/4'.
-module(cowboy_metrics).

-include("cm_snmp.hrl").

-export([start_http/4, start_http/5]).
-export([create_server/1]).


%% @doc Start a cowboy http listener. This is an identical function to
%% `cowboy:start_http/4'. It is also is an equivalent to
%% `start_http(1, Ref, Acceptors, TransOpts, ProtoOpts)'. It starts
%% the metrics with a service of index equals to 1.
%% @see //cowboy/cowboy:start_http/4
%% @see //cowboy_metrics/cowboy_metrics:start_http/5
-spec start_http(ranch:ref(), non_neg_integer(), ranch_tcp:opts(),
                 cowboy_protocol:opts()) -> {ok, pid()} | {error, any()}.
start_http(Ref, Acceptors, TransOpts, PorotoOpts) ->
    start_http(1, Ref, Acceptors, TransOpts, PorotoOpts).


%% @doc A helper function that will create a cowboy server with some
%% SNMP service information. It will also set the appropriate
%% onrequest and onresponse cowboy hooks. This function will detect
%% existing cowboy hooks and will compose them with the SNMP hooks.
-spec start_http(pos_integer(), ranch:ref(), non_neg_integer(),
                 ranch_tcp:opts(), cowboy_protocol:opts())
                -> {ok, pid()} | {error, any()}.
start_http(Index, Ref, Acceptors, TransOpts, ProtoOpts) ->
    Svc = #cm_svc{index   = Index
                 ,name    = ref_string(Ref)
                 ,description = "COWBOY Web Server"},
    {ok, ReqFun, RespFun} = create_server(Svc),
    HooksOpts = update_hooks(ProtoOpts, ReqFun, RespFun),
    case cowboy:start_http(Ref, Acceptors, TransOpts, HooksOpts) of
        {ok, _} = Ok ->
            Port = ranch:get_port(Ref),
            true = cm_www_mib:update_service_port(Index, Port),
            Ok;
        Other ->
            Other
    end.


%% @doc Create a service entry in the SNMP service information
%% table. This function returns the appropriate
%% `cowboy:onrequest_fun/1' and `cowboy:onresponse_fun/4'.
-spec create_server(#cm_svc{}) ->
                           {ok, cowboy:onrequest_fun(),
                                cowboy:onresponse_fun()} |
                           {error, term()}.
create_server(Svc = #cm_svc{index = Idx}) ->
    case cm_www_mib:create_service_entry(Svc) of
        true ->
            ok = gen_server:call(cowboy_metrics_server, {init_svc, Idx}),
            {ok
            ,fun(Req) -> on_request(Idx, Req) end
            ,fun(Status, Headers, Body, Req) ->
                     on_response(Idx, Status, Headers, Body, Req)
             end};
        false ->
            {error, bad_svc_record}
    end.


-spec on_request(non_neg_integer(), cowboy_req:req()) -> cowboy_req:req().
on_request(SvcId, Req) ->
    {Method, Req1} = cowboy_req:method(Req),
    Method1 = binary_to_existing_atom(Method, latin1),
    {Size, Req3} = case cowboy_req:body_length(Req1) of
                       {undefined, Req2} -> {0, Req2};
                       {Length, Req2} -> {Length, Req2}
                   end,
    gen_server:cast(cowboy_metrics_server, {request, SvcId, Method1, Size}),
    Req3.


-spec on_response(non_neg_integer(), cowboy:http_status(),
                  cowboy:http_headers(),
                  iodata(), cowboy_req:req()) -> cowboy_req:req().
on_response(SvcId, Status, _, Body, Req) ->
    Size = byte_size(Body),
    gen_server:cast(cowboy_metrics_server, {response, SvcId, Status, Size}),
    Req.



ref_string(Ref) when is_binary(Ref) ->
    binary_to_list(Ref);
ref_string(Ref) when is_atom(Ref) ->
    atom_to_list(Ref);
ref_string(Ref) when is_list(Ref) ->
    Ref.


update_hooks(Opts, ReqFun, RespFun) ->
    update_resp_hook(update_req_hook(Opts, ReqFun), RespFun).


update_req_hook(Opts, ReqFun) ->
    case lists:keytake(onrequest, 1, Opts) of
        {value, {onrequest, Fun}, Opts1} ->
            [{onrequest, fun (Req) -> ReqFun(Fun(Req)) end} | Opts1];
        false ->
            [{onrequest, ReqFun} | Opts]
    end.


update_resp_hook(Opts, RespFun) ->
    case lists:keytake(onresponse, 1, Opts) of
        {value, {onresponse, Fun}, Opts1} ->
            [{onresponse, fun (S, H, B, Req) ->
                                  RespFun(S, H, B, Fun(S, H, B, Req))
                          end} | Opts1];
        false ->
            [{onresponse, RespFun} | Opts]
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

update_hooks_single_test_() ->
    ReqFun = fun(_) -> req_fun end,
    RespFun = fun (_, _, _, _) -> resp_fun end,

    Updated = update_hooks([{some, opt}], ReqFun, RespFun),
    {onrequest, NewReqFun} = lists:keyfind(onrequest, 1, Updated),
    {onresponse, NewRespFun} = lists:keyfind(onresponse, 1, Updated),
    [
     ?_assertEqual(3, length(Updated)),
     ?_assertMatch(req_fun, NewReqFun(foo)),
     ?_assertMatch(resp_fun, NewRespFun(a, b, c, d))
    ].


update_hooks_test_() ->
    ReqFun = fun(X) -> X + 1 end,
    RespFun = fun(A, B, C, D) -> lists:sum([A, B, C, D]) end,
    
    Opts = [{some, opt}, {onrequest, ReqFun}, {onresponse, RespFun}],
    Updated = update_hooks(Opts, ReqFun, RespFun),
    ?debugVal(Updated),
    {onrequest, NewReqFun} = lists:keyfind(onrequest, 1, Updated),
    {onresponse, NewRespFun} = lists:keyfind(onresponse, 1, Updated),
    ?debugVal(NewReqFun),
    [
     ?_assertEqual(3, length(Updated)),
     ?_assertEqual(2, ReqFun(ReqFun(0))),
     ?_assertEqual(7, RespFun(1, 1, 1, RespFun(1, 1, 1, 1))),
     ?_assertEqual(2, NewReqFun(0)),
     ?_assertEqual(7, NewRespFun(1,1,1,1))
    ].

-endif.
