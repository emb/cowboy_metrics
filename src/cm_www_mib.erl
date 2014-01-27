%% Copyright (c) 2013-2015, Ed Mohamed Barwani <ed.barwani@gmail.com>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are
%% met:
%%
%% 1. Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%%
%% 2. Redistributions in binary form must reproduce the above
%% copyright notice, this list of conditions and the following
%% disclaimer in the documentation and/or other materials provided
%% with the distribution.
%%
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

%% @doc Implement WWW-MIB from RFC-2594
-module(cm_www_mib).

-type request_type() :: 'OPTIONS'
                      | 'GET'
                      | 'HEAD'
                      | 'POST'
                      | 'PUT'
                      | 'PATCH'
                      | 'DELETE'.

-export_type([request_type/0]).

%% API
-export([load/0, load/1]).
-export([unload/0, unload/1]).

-export([create_service_entry/1,
         create_req_entry/2]).
-export([request_in/1]).

%% SNMP Instrumentation functions
-export([summary_table/1, summary_table/3]).


-include("cm_snmp.hrl").
-include("NETWORK-SERVICES-MIB.hrl").
-include("WWW-MIB.hrl").

-define(MIB, "WWW-MIB").

%% FIXME: Assuming that we are always using volatile databases for
%% these tables. Maybe this should be configurable in the future.
-define(SVC_DB, {wwwServiceTable, volatile}).
-define(REQ_DB, {wwwRequestInTable, volatile}).

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


-spec create_service_entry(#cm_svc{}) -> true | false | {error, term()}.
create_service_entry(Svc = #cm_svc{index = Index}) ->
    %% Refer WwwServiceEntry in WWW-MIB.
    Row = {Index,
           Svc#cm_svc.description,
           Svc#cm_svc.contact,
           ?applTCPProtoID ++ [Svc#cm_svc.port],
           Svc#cm_svc.name,
           service_type(Svc#cm_svc.type),
           "00000000000", %% FIXME: StartTime (unknown)
           2,             %% FIXME: Operation Status (running)
           "00000000000"  %% FIXME: LastChange (unknown)
          },
    snmpa_mib_lib:table_cre_row(?SVC_DB, [Index], Row).


-spec create_req_entry(non_neg_integer(), request_type()) ->
                              true | false | {error, term()}.
create_req_entry(SvcIndex, Method) ->
    RowIndex = to_rowindex([SvcIndex, Method]),
    Row = {RowIndex,
           0,              %% InRequests
           0,              %% InBytes
           "00000000000"}, %% InLastTime
    snmpa_mib_lib:table_cre_row(?REQ_DB, RowIndex, Row).


-spec request_in(#cm_req{}) -> true | false.
request_in(Req = #cm_req{svc_index = Idx, type = Method}) ->
    RowIndex = to_rowindex([Idx, Method]),
    Cols = [?wwwRequestInRequests, ?wwwRequestInBytes],

    [Count, Size] = snmp_generic:table_get_elements(?REQ_DB, RowIndex, Cols),

    SetCols = [{?wwwRequestInRequests, counter32_inc(Count)}
              ,{?wwwRequestInBytes, counter32_inc(Size, Req#cm_req.size)}
              ,{?wwwRequestInLastTime, to_datetime(Req#cm_req.timestamp)}],

    snmp_generic:table_set_elements(?REQ_DB, RowIndex, SetCols).


%% Instrumentation fuctions

%% ignore new/delete operations.
summary_table(_) ->
    ok.

summary_table(get, RowIndex, Cols) ->
    [get_summary(RowIndex, Col) || Col <- Cols];

summary_table(get_next, _RowIndex, []) ->
    [];
summary_table(get_next, RowIndex, Cols) ->
    [get_next_summary(RowIndex, Col) || Col <- Cols];

summary_table(is_set_ok, _RowIndex, [{Col, _} | _]) ->
    {noAccess, Col}.

%% Internal helpers

service_type(other) -> ?wwwServiceType_wwwOther;
service_type(server) -> ?wwwServiceType_wwwServer;
service_type(client) -> ?wwwServiceType_wwwClient;
service_type(proxy) -> ?wwwServiceType_wwwProxy;
service_type(caching_proxy) -> ?wwwServiceType_wwwCachingProxy.


to_rowindex(Indices) ->
    lists:foldr(fun to_rowindex/2, [], Indices).

to_rowindex(Elem, Acc) when is_number(Elem) ->
    [Elem | Acc];
to_rowindex(Elem, Acc) when is_atom(Elem) ->
    to_rowindex(atom_to_list(Elem), Acc);
to_rowindex(Elem, Acc) when is_list(Elem) ->
    lists:flatten([length(Elem), Elem, Acc]).


counter32_inc(Value) ->
    counter32_inc(Value, 1).

counter32_inc(Value, By) ->
    (Value + By) rem 4294967296.


to_datetime(DateTime) ->
    snmp:universal_time_to_date_and_time(DateTime).


lower_32(Number) ->
    case binary:encode_unsigned(Number) of
        Short when byte_size(Short) =< 4 ->
            Number;
        Long ->
            %% Get the lower 4 bytes (32-bits)
            Lower = binary:part(Long, {byte_size(Long), -4}),
            binary:decode_unsigned(Lower)
    end.


count_requests(DB, SvcIndex) ->
    %% Requests RowIndex is a combination of service id & request
    %% type.
    Fun = fun (_OID, {[Index| _], Count, _, _}, Acc) when Index =:= SvcIndex->
                  counter32_inc(Acc, Count);
              (_, _, Acc) ->
                  Acc
          end,
    table_fold(DB, Fun, 0).


count_bytes(DB, SvcIndex) ->
    Fun = fun (_OID, {[Index| _], _, Bytes, _}, Acc) when Index =:= SvcIndex->
                  Acc + Bytes;
              (_, _, Acc) ->
                  Acc
          end,
    table_fold(DB, Fun, 0).


get_summary([SvcIndex], ?wwwSummaryInRequests) ->
    {value, count_requests(?REQ_DB, SvcIndex)};

get_summary([SvcIndex], ?wwwSummaryInBytes) ->
    {value, count_bytes(?REQ_DB, SvcIndex)};

get_summary([SvcIndex], ?wwwSummaryInLowBytes) ->
    Total = count_bytes(?REQ_DB, SvcIndex),
    {value, lower_32(Total)};

get_summary(_RowIndex, _) ->
    {noValue, noSuchObject}.


get_next_summary(SvcIndex, Col) ->
    get_next_summary(SvcIndex, Col, ?wwwSummaryInRequests,
                     ?wwwSummaryOutLowBytes).

get_next_summary(RowIndex, Col, FirstCol, LastCol) ->
    case snmp_generic:table_next(?SVC_DB, RowIndex) of
        endOfTable when Col >= LastCol ->
            endOfTable;
        endOfTable ->
            get_next_summary([], Col + 1, FirstCol, LastCol);
        [SvcIndex] ->
            case get_summary([SvcIndex], Col) of
                {value, Value} ->
                    {[Col | [SvcIndex]], Value};
                {noValue, _} ->
                    get_next_summary([SvcIndex], Col, FirstCol, LastCol)
            end
    end.


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



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

to_rowindex_test_() ->
    [
     ?_assertMatch([3], to_rowindex([3])),
     ?_assertMatch([2, $a, $b], to_rowindex([ab])),
     ?_assertMatch([1, 2, $a, $b], to_rowindex([1, ab])),
     ?_assertMatch([1, 3, $G, $E, $T, 2], to_rowindex([1, "GET", 2]))
    ].


lower_32_test_() ->
    [
     ?_assertEqual(5, lower_32(5)),
     ?_assertEqual(573, lower_32(573)),

     ?_assertEqual(16843009, lower_32(16843009)),
     ?_assertEqual(16843009, lower_32(4311810305)),

     ?_assertEqual(33686018, lower_32(33686018)),
     ?_assertEqual(33686018, lower_32(8623620610))
    ].

-endif.
