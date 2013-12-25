%% Copyright (c) 2013-2015, Ed Mohmaed Barwani <ed.barwani@gmail.com>
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

%% @doc A gen_server process that accepts instrumentation messages.
-module(cowboy_metrics_server).
-behaviour(gen_server).

%% gen_server callbacks.
-export([init/1,
	 handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2,
	 code_change/3]).

%% API
-export([start_link/0, start_link/1]).
-export([incr_request/1]).

-record(state, {backend, module}).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    start_link([]).


-spec start_link(list()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).


%% @doc Increment requests counter
-spec incr_request(binary()) -> ok.
incr_request(Method) ->
    gen_server:cast(?SERVER, {request, Method}).


%% gen_server callbacks
init(Opts) ->
    Backend = proplists:get_value(backend, Opts, snmp),
    Module = backend_module(Backend),
    case Module:init() of
	ok ->
	    {ok, #state{backend = Backend, module = Module}};
	Error ->
	    {stop, Error}
    end.


handle_call(_Request, _From, State) ->
    error_logger:warning_msg("unknown request: ~p from: ~p", [_Request, _From]),
    {noreply, State}.


handle_cast({request, Method}, State = #state{module = Module}) ->
    Module:increment_request(Method),
    {noreply, State};
handle_cast(_Message, State) ->
    error_logger:warning_msg("unknown message: ~p", [_Message]),
    {noreply, State}.


handle_info(_Message, State) ->
    error_logger:warning_msg("unknown message: ~p", [_Message]),
    {noreply, State}.


terminate(_Reason, #state{module = Module}) ->
    Module:terminate(),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% helpers
backend_module(snmp) -> cowboy_metrics_snmp.
