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

%% @doc Cowboy mib implementation
-module(cowboy_metrics_snmp).
-behaviour(gen_server).

-include("COWBOY-MIB.hrl").

%% gen_server callbacks.
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%% API
-export([start_link/0]).
-export([load/0,   load/1]).
-export([unload/0, unload/1]).
-export([notify_requst/1]).
-export([notify_response/2]).

%% SNMP Accessors
-export([total_requests/1]).
-export([total_responses/1, total_response_size/1]).
-export([table/2, table/4]).


-define(SERVER, cowboy_metrics_server).
-define(MIB, "COWBOY-MIB").
-define(REQ_DB, {requestTable, volatile}).
-define(RESP_DB, {responseTable, volatile}).

-record(state, {
         }).


-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% @doc load mibs to the default master agent.
-spec load() -> ok | {error, term()}.
load() ->
    load(snmp_master_agent).


-spec load(pid() | atom()) -> ok | {error, term()}.
load(Agent) ->
    Path = [code:priv_dir(cowboy_metrics), "mibs", ?MIB],
    snmpa:load_mib(Agent, filename:join(Path)).


%% @doc unload mib from the default agent.
-spec unload() -> ok | {error, term()}.
unload() ->
    unload(snmp_master_agent).


-spec unload(pid() | atom()) -> ok | {error, term()}.
unload(Agent) ->
    snmpa:unload_mib(Agent, ?MIB).


-spec notify_requst(string() | binary()) -> ok.
notify_requst(Method) ->
    gen_server:cast(?SERVER, {request, Method}).


-spec notify_response(non_neg_integer() | binary(), non_neg_integer()) -> ok.
notify_response(Code, Size) ->
    gen_server:cast(?SERVER, {response, Code, Size}).


total_requests(get) ->
    gen_server:call(?SERVER, get_total_requests);
total_requests(_) ->
    ok.


total_responses(get) ->
    gen_server:call(?SERVER, get_total_responses);
total_responses(_) ->
    ok.


total_response_size(get) ->
    gen_server:call(?SERVER, get_total_response_size);
total_response_size(_) ->
    ok.


%% Pass through operations unchanged. We are going to use the default
%% agent storage options.
table(Op, DB) ->
    snmp_generic:table_func(Op, DB).


table(Op, RowIndex, Cols, DB) ->
    snmp_generic:table_func(Op, RowIndex, Cols, DB).


%% gen_server callbacks

init(_Opts) ->
    case load() of
        ok ->
            create_rows(?REQ_DB, methods()),
            create_rows(?RESP_DB, code_classes()),
            {ok, #state{}};
        {error,already_loaded} ->
            {ok, #state{}};
        Error ->
            {stop, Error}
    end.


handle_call(get_total_requests, _From, State) ->
    Fun = fun (_OID, {_Method, Count}, Acc) -> counter32_inc(Acc, Count) end,
    Total = table_fold(?REQ_DB, Fun, 0),
    {reply, {value, Total}, State};
handle_call(get_total_responses, _From, State) ->
    Fun = fun (_OID, {_Code, Count, _Size}, Acc) ->
                  counter32_inc(Acc, Count)
          end,
    Total = table_fold(?RESP_DB, Fun, 0),
    {reply, {value, Total}, State};
handle_call(get_total_response_size, _From, State) ->
    Fun = fun (_OID, {_Code, _Count, Size}, Acc) ->
                  counter32_inc(Acc, Size)
          end,
    Total = table_fold(?RESP_DB, Fun, 0),
    {reply, {value, Total}, State};
handle_call(_Request, _From, State) ->
    error_logger:warning_msg("unknown request: ~p from: ~p", [_Request, _From]),
    {noreply, State}.


handle_cast({request, Method}, State) when is_binary(Method) ->
    handle_cast({request, binary_to_list(Method)}, State);
handle_cast({request, Method}, State) ->
    RowIndex = row_index(Method),
    {value, Count} = snmp_generic:table_get_element(?REQ_DB, RowIndex,
                                                    ?requestMethodCount),
    snmp_generic:table_set_element(?REQ_DB, RowIndex, ?requestMethodCount,
                                   counter32_inc(Count)),
    {noreply, State};
handle_cast({response, Code, Size}, State) ->
    Class = status_class(Code),
    RowIndex = row_index(Class),
    Cols = [?responseCodeCount, ?responseCodeSize],
    [Count, OldSize] = snmp_generic:table_get_elements(?RESP_DB, RowIndex,
                                                       Cols),

    SetCols = [{?responseCodeCount, counter32_inc(Count)},
               {?responseCodeSize, counter32_inc(OldSize, Size)}],
    snmp_generic:table_set_elements(?RESP_DB, RowIndex, SetCols),
    {noreply, State};
handle_cast(_Message, State) ->
    error_logger:warning_msg("unknown message: ~p", [_Message]),
    {noreply, State}.


handle_info(_Message, State) ->
    error_logger:warning_msg("unknown message: ~p", [_Message]),
    {noreply, State}.


terminate(_Reason, _State) ->
    unload(),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% helpers

methods() ->
    [{"GET", 0},
     {"HEAD", 0},
     {"POST", 0},
     {"PUT", 0},
     {"PATCH", 0},
     {"DELETE", 0},
     {"OPTIONS", 0}].


code_classes() ->
    [{"1xx", 0, 0},
     {"2xx", 0, 0},
     {"3xx", 0, 0},
     {"4xx", 0, 0},
     {"5xx", 0, 0}].


create_rows(DB, [Row | Tail]) ->
    Key = element(1, Row),
    true = snmpa_mib_lib:table_cre_row(DB, row_index(Key), Row),
    create_rows(DB, Tail);
create_rows(_, []) ->
    ok.


%% String indexes should have length as prefix, se rfc2578 7.7
row_index(Method) ->
    [length(Method) | Method].


counter32_inc(Value) ->
    counter32_inc(Value, 1).

counter32_inc(Value, By) ->
    (Value + By) rem 4294967296.


status_class(Status) when is_binary(Status) ->
    status_class(binary_to_list(Status));
status_class(Status) when is_integer(Status) ->
    status_class(integer_to_list(Status));
status_class([Class | _]) ->
    [Class, $x, $x].


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
