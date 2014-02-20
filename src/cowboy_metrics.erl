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

%% @doc Entry module to this application.
-module(cowboy_metrics).

-include("cm_snmp.hrl").

-export([create_server/1]).

%% @doc Create a webserver metrics table. This function returns the
%% appropriate `cowboy:onrequest_fun()' & `cowboy:onresponse_fun()'.
-spec create_server(#cm_svc{}) ->
                           {ok, cowboy:onrequest_fun(), cowboy:onresponse_fun()} |
                           {error, term()}.
create_server(Svc = #cm_svc{index = Idx}) ->
    case cm_www_mib:create_service_entry(Svc) of
        true ->
            gen_server:call(cowboy_metrics_server, {init_svc, Idx}),
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
