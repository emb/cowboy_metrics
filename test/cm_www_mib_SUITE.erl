-module(cm_www_mib_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("snmp/include/snmp_types.hrl").
-include_lib("../include/WWW-MIB.hrl").
-include_lib("../include/NETWORK-SERVICES-MIB.hrl").
-include_lib("../include/cm_snmp.hrl").

%% ct callbacks.
-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).

%% tests
-export([
         create_svc_entry/1,
         create_req_entry/1,
         update_requests/1,
         get_reqs_summary_values/1
        ]).


suite() ->
    %% SNMP Config
    [
     {require, snmp_mgr_agent, snmp},
     {require, snmp_app, snmp_app}
    ].


all() ->
    [
     {group, serviceTable},
     {group, summaryTable},
     {group, requestTable}
    ].


groups() ->
    [
     {serviceTable, [], [create_svc_entry]},
     {requestTable, [], [create_req_entry, update_requests]},
     {summaryTable, [], [get_reqs_summary_values]}
    ].


init_per_suite(Config) ->
    ok = ct_snmp:start(Config, snmp_mgr_agent, snmp_app),
    ok = cm_www_mib:load(),
    Config.


end_per_suite(Config) ->
    ok = cm_www_mib:unload(),
    ct_snmp:stop(Config),
    Config.


init_per_group(requestTable, Config) ->
    [{svc_index, 2}
    ,{method, 'OPTIONS'}
    ,{row_index, [2, 7, $O, $P, $T, $I, $O, $N, $S]}
     | Config];

init_per_group(_, Config) ->
    Config.


end_per_group(_, Config) ->
    Config.


-define(serviceEntries(Key),
        [
         ?wwwServiceEntry ++ [?wwwServiceDescription] ++ [Key],
         ?wwwServiceEntry ++ [?wwwServiceContact] ++ [Key],
         ?wwwServiceEntry ++ [?wwwServiceProtocol] ++ [Key],
         ?wwwServiceEntry ++ [?wwwServiceName] ++ [Key],
         ?wwwServiceEntry ++ [?wwwServiceType] ++ [Key]
        ]).

create_svc_entry(_Config) ->
    Svc = #cm_svc{index = 4,
                  description = "create_svc_entry test",
                  contact = "cm_www_mib_SUITE",
                  name = "test@cowboy_metrics",
                  port = 8080,
                  type = proxy},

    true = cm_www_mib:create_service_entry(Svc),

    {noError, 0, Variables} = ct_snmp:get_values(cowboy_mib_test,
                                                 ?serviceEntries(
                                                    Svc#cm_svc.index),
                                                 snmp_mgr_agent),

    [Desc, Contact, Protocol, Name, Type] = Variables,

    check_value(Svc#cm_svc.description, Desc),
    check_value(Svc#cm_svc.contact, Contact),

    ProtoID = ?applTCPProtoID ++ [Svc#cm_svc.port],
    check_value(ProtoID, Protocol),

    check_value(Svc#cm_svc.name, Name),
    check_value(?wwwServiceType_wwwProxy, Type).


-define(requestEntries(Index),
        [
         ?wwwRequestInEntry ++ [?wwwRequestInRequests | Index],
         ?wwwRequestInEntry ++ [?wwwRequestInBytes | Index],
         ?wwwRequestInEntry ++ [?wwwRequestInLastTime | Index]
        ]).

create_req_entry(Config) ->
    SvcIndex = ?config(svc_index, Config),
    Method = ?config(method, Config),
    Index = ?config(row_index, Config),

    true = cm_www_mib:create_req_entry(SvcIndex, Method),

    {noError, 0, Variables} = ct_snmp:get_values(cowboy_mib_test,
                                                 ?requestEntries(Index),
                                                 snmp_mgr_agent),

    [Reqs, Bytes, _] = Variables,

    check_value(0, Reqs),
    check_value(0, Bytes).


update_requests(Config) ->
    SvcIndex = ?config(svc_index, Config),
    Method = ?config(method, Config),
    Index = ?config(row_index, Config),
    Time = calendar:universal_time(),

    true = cm_www_mib:request_in(#cm_req{
                                    svc_index = SvcIndex,
                                    size = 12,
                                    type = Method,
                                    timestamp = Time
                                   }),

    {noError, 0, Variables} = ct_snmp:get_values(cowboy_mib_test,
                                                 ?requestEntries(Index),
                                                 snmp_mgr_agent),
    [Reqs, Bytes, DateAndTime] = Variables,

    check_value(1, Reqs),
    check_value(12, Bytes),
    check_value(snmp:universal_time_to_date_and_time(Time), DateAndTime).



-define(summuryEntries(Key),
       [
        ?wwwSummaryEntry ++ [?wwwSummaryInRequests] ++ [Key],
        ?wwwSummaryEntry ++ [?wwwSummaryOutRequests] ++ [Key],
        ?wwwSummaryEntry ++ [?wwwSummaryInResponses] ++ [Key],
        ?wwwSummaryEntry ++ [?wwwSummaryOutResponses] ++ [Key],
        ?wwwSummaryEntry ++ [?wwwSummaryInBytes] ++ [Key],
        ?wwwSummaryEntry ++ [?wwwSummaryInLowBytes] ++ [Key],
        ?wwwSummaryEntry ++ [?wwwSummaryOutBytes] ++ [Key],
        ?wwwSummaryEntry ++ [?wwwSummaryOutLowBytes] ++ [Key]
       ]).

get_reqs_summary_values(_Config) ->
    Index = 4,
    Reqs = [{'OPTIONS', 15}, {'OPTIONS', 10}
           ,{'GET', 31},     {'GET', 0}
           ,{'HEAD', 45},    {'HEAD', 0}
           ,{'PUT', 89},     {'PUT', 1029}
           ,{'POST', 12},    {'POST', 182}
           ,{'PATCH', 10},   {'PATCH', 0}
           ,{'DELETE', 79},  {'DELETE', 0}],
    Time = calendar:universal_time(),
    Req = #cm_req{timestamp = Time, svc_index = Index},

    Methods = ['OPTIONS', 'GET', 'HEAD', 'PUT', 'POST', 'PATCH', 'DELETE'],

    %% Create rows!
    lists:foreach(fun (Method) ->
                          true = cm_www_mib:create_req_entry(Index, Method)
                  end, Methods),

    %% Now create entries
    lists:foreach(fun ({Type, Size}) ->
                          true = cm_www_mib:request_in(Req#cm_req{type = Type,
                                                                  size = Size})
                  end, Reqs),

    {noError, 0, Variables} = ct_snmp:get_values(cowboy_mib_test,
                                                 ?summuryEntries(Index),
                                                 snmp_mgr_agent),
    [Count, _, _, _, Bytes, LowerBytes, _, _] = Variables,

    %% Tests!
    check_value(length(Reqs), Count),
    Total = lists:foldl(fun ({_, S}, Acc) -> S + Acc end, 0, Reqs),
    check_value(Total, Bytes),
    check_value(Total, LowerBytes).


check_value(Expect, #varbind{oid = OID, value = Value}) ->
    ct:log(info, "varbind -> OID: ~p expected: ~p value: ~p",
           [OID, Expect, Value]),
    Expect = Value.
