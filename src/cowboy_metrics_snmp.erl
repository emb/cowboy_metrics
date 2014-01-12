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


%% gen_server callbacks.
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%% API
-export([start_link/0]).
-export([notify_requst/1]).
-export([notify_response/2]).

%% REMOVE ME
-export([table_fold/3, counter32_inc/1]).

-define(SERVER, cowboy_metrics_server).

-record(state, {
         }).


-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


-spec notify_requst(string() | binary()) -> ok.
notify_requst(Method) ->
    gen_server:cast(?SERVER, {request, Method}).


-spec notify_response(non_neg_integer() | binary(), non_neg_integer()) -> ok.
notify_response(Code, Size) ->
    gen_server:cast(?SERVER, {response, Code, Size}).


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


handle_call(_Request, _From, State) ->
    error_logger:warning_msg("unknown request: ~p from: ~p", [_Request, _From]),
    {noreply, State}.


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


%% helpers

counter32_inc(Value) ->
    counter32_inc(Value, 1).

counter32_inc(Value, By) ->
    (Value + By) rem 4294967296.


%% TODO: This function belongs in snmpa_generic moudle. Should submit
%% patch? This is inspired by snmpa_generic:table_foreach.
-spec table_fold(tuple(),
                 fun((OID::list(), Row::term(), AccIn) -> AccOut),
                 AccIn) -> AccOut when AccOut::term(), AccIn::term().
table_fold(Table, Fun, AccIn) ->
    table_fold(Table, Fun, AccIn, undefined).

table_fold(Table, Fun, AccIn, FOI) ->
    table_fold(Table, Fun, AccIn, FOI, []).

table_fold(Table, Fun, AccIn, FOI, OID) ->
    case snmp_generic:table_next(Table, OID) of
        endOfTable ->
            AccIn;
        OID ->
            %% Not really next :-(
            exit({cyclic_db_reference, OID});
        NextOID ->
            AccOut = case snmp_generic:table_get_row(Table, NextOID, FOI) of
                         undefined ->
                             AccIn;
                         Row ->
                             Fun(NextOID, Row, AccIn)
                     end,
            table_fold(Table, Fun, AccOut, FOI, NextOID)
    end.
