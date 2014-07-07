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

%% @doc Cowboy mib implementation
-module(cowboy_metrics_snmp).
-behaviour(gen_server).

-include("cm_snmp.hrl").

%% gen_server callbacks.
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%% API
-export([start_link/0]).

-define(SERVER, cowboy_metrics_server).
-define(METHODS, ['GET', 'HEAD', 'OPTIONS', 'PUT', 'POST', 'PATCH', 'DELETE']).
-record(state, {}).


-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% gen_server callbacks

init(_Opts) ->
    case cm_www_mib:load() of
        ok ->
            {ok, #state{}};
        {error,already_loaded} ->
            {ok, #state{}};
        Error ->
            {stop, Error}
    end.

handle_call({init_svc, SvcIndex}, _From, State) ->
    lists:foreach(fun (M) ->
                          true = cm_www_mib:create_req_entry(SvcIndex, M)
                  end, ?METHODS),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    error_logger:warning_msg("unknown request: ~p from: ~p", [_Request, _From]),
    {noreply, State}.


handle_cast(M = {request, SvcIdx, Method, Size}, State) ->
    Req = #cm_req{svc_index = SvcIdx, type = Method, size = Size},
    case cm_www_mib:request_in(Req) of
        true  -> ok;
        false -> error_logger:warning_msg("failed snmp update: ~p", [M])
    end,
    {noreply, State};

handle_cast({response, SvcIdx, Status, Size}, State) when is_binary(Status)->
    <<BinStatus:3/binary, $ , _/binary>> = Status,
    handle_cast({response, SvcIdx, binary_to_integer(BinStatus), Size}, State);

handle_cast(M = {response, SvcIdx, Status, Size}, State) ->
    Resp = #cm_resp{svc_index = SvcIdx, code = Status, size = Size},
    case cm_www_mib:response_out(Resp) of
        true  -> ok;
        false -> error_logger:warning_msg("failed snmp update: ~p", [M])
    end,
    {noreply, State};

handle_cast(_Message, State) ->
    error_logger:warning_msg("unknown message: ~p", [_Message]),
    {noreply, State}.


handle_info(_Message, State) ->
    error_logger:warning_msg("unknown message: ~p", [_Message]),
    {noreply, State}.


terminate(_Reason, _State) ->
    cm_www_mib:unload(),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



