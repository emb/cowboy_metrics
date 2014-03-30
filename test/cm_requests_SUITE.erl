%% Create end-to-end tests of the cowboy server.
-module(cm_requests_SUITE).

-include("../include/cm_snmp.hrl").
-include("../include/WWW-MIB.hrl").
-include_lib("../include/NETWORK-SERVICES-MIB.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("snmp/include/snmp_types.hrl").


%% ct callbacks.
-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).

%% Tests
-export([
         req_svcs_dont_interfere/1
        ,start_http/1
        ,'cowboy_metrics#3_response_size_calculation'/1
        ]).


suite() ->
    %% SNMP Config
    [
     {require, snmp_mgr_agent, snmp}
    ,{require, snmp_app, snmp_app}
    ].


all() ->
    [
     {group, two_cowboys}
    ,{group, start_tests}
    ,{group, hooks}
    ].


groups() ->
    [
     {two_cowboys, [], [req_svcs_dont_interfere]}
    ,{start_tests, [], [start_http]}
    ,{hooks, [], ['cowboy_metrics#3_response_size_calculation']}
    ].


init_per_suite(Config) ->
    ok = ct_snmp:start(Config, snmp_mgr_agent, snmp_app),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(cowboy_metrics),
    ok = application:ensure_started(ibrowse),

    Config.


end_per_suite(Config) ->
    application:stop(ibrowse),
    application:stop(cowboy_metrics),
    application:stop(cowboy),
    ct_snmp:stop(Config),
    Config.


init_per_group(two_cowboys, Config) ->
    {ok, Config1} = start_cowboy(idx(dee), dee, Config),
    {ok, Config2} = start_cowboy(idx(dum), dum, Config1),
    Config2;

init_per_group(start_tests, Config) ->
    Dispatch = cowboy_router:compile([{'_', [
                                             {"/", test_handler, []}
                                            ]}
                                     ]),
    Name = start_tests_cowboy,
    Index = 2,
    {ok, _} = cowboy_metrics:start_http(Index, Name, 100, [{port, 0}],
                                        [{env, [{dispatch, Dispatch}]}]),
    [{name, Name}, {index, Index} | Config];

init_per_group(hooks, Config) ->
    Dispatch = cowboy_router:compile([{'_',
                                       [
                                        {"/", test_response_type_handler, []}
                                       ]}
                                     ]),
    Name = hooks,
    {ok, _} = cowboy_metrics:start_http(3, Name, 100, [{port, 0}],
                                        [{env, [{dispatch, Dispatch}]}]),
    Port = ranch:get_port(Name),
    [{name, Name}, {port, Port} | Config].


end_per_group(two_cowboys, Config) ->
    cowboy:stop_listener(dee),
    cowboy:stop_listener(dum),
    Config;

end_per_group(Group, Config)
  when Group == start_tests orelse
       Group == hooks ->
    cowboy:stop_listener(?config(name, Config)),
    Config;

end_per_group(_, Config) ->
    Config.


%% Tests
req_svcs_dont_interfere(Config) ->
    %% 2 Requests
    {ok, _} = send(dee, get, 200, Config),
    {ok, _} = send(dee, options, 300, Config),
    %% 1 Requset
    {ok, _} = send(dum, post, 404, Config),
    %% Check summary
    2 = summary(dee, ?wwwSummaryInRequests),
    2 = summary(dee, ?wwwSummaryOutResponses),
    1 = summary(dum, ?wwwSummaryInRequests),
    1 = summary(dum, ?wwwSummaryOutResponses),
    %% Check reqs/responses table
    1 = reqs(dee, "GET"),
    1 = reqs(dee, "OPTIONS"),
    1 = reqs(dum, "POST"),
    1 = resps(dee, 200),
    1 = resps(dee, 300),
    1 = resps(dum, 404).


start_http(Config) ->
    Port = ranch:get_port(?config(name, Config)),
    OID = ?wwwServiceEntry ++ [?wwwServiceProtocol, ?config(index, Config)],
    SnmpPort = snmp_value(OID),

    SnmpPort = ?applTCPProtoID ++ [Port].


'cowboy_metrics#3_response_size_calculation'(doc) ->
    ["Ensure that our onresponse hook doesn't crash."];

'cowboy_metrics#3_response_size_calculation'(Config) ->
    %% Ensure binary body does not crash cowboy server.
    {ok, "200", _, _} = http_get([{"Accept", "text/binary+plain"}], Config),
    %% iolist?
    {ok, "200", _, _} = http_get([{"Accept", "text/iolist+plain"}], Config),
    %% Other than iodata means that cowboy itself will crash, not the
    %% fault of our snmp on response hook.
    {error, retry_later} = http_get([{"Accept", "text/other+plain"}], Config).


%% Helpers
start_cowboy(Idx, Name, Config) ->
    Dispatch = cowboy_router:compile([{'_', [
                                             {"/", test_handler, []}
                                            ]}
                                     ]),
    Svc = #cm_svc{index   = Idx
                 ,name    = atom_to_list(Name)
                 ,contact = atom_to_list(?MODULE)
                 ,description = "Running some tests"},
    {ok, ReqFun, RespFun} = cowboy_metrics:create_server(Svc),
    {ok, _} = cowboy:start_http(Name, 100, [{port, 0}],
                                [{env, [{dispatch, Dispatch}]}
                                ,{onrequest, ReqFun}
                                ,{onresponse, RespFun}
                                ]),
    Port = ranch:get_port(Name),
    {ok, [{{Name, port}, Port} | Config]}.


uri(Config) ->
    Port = ?config(port, Config),
    lists:flatten(io_lib:format("http://localhost:~p/", [Port])).


uri(Name, Config) ->
    Port = ?config({Name, port}, Config),
    uri([{port, Port}]).


http_get(Headers, Config) ->
    ibrowse:send_req(uri(Config), Headers, get).


send(Name, Method, Code, Config) when is_integer(Code) ->
    send(Name, Method, integer_to_list(Code), Config);
send(Name, Method, Code, Config) when is_list(Code)->
    Uri = uri(Name, Config) ++ "?code=" ++ Code,
    {ok, Code, _, _} = ibrowse:send_req(Uri, [], Method),
    {ok, Code}.


idx(dee) -> 1;
idx(dum) -> 2.


summary(Name, Counter) ->
    OID = ?wwwSummaryEntry ++ [Counter, idx(Name)],
    snmp_value(OID).


reqs(Name, Method) ->
    OID = ?wwwRequestInEntry ++ [?wwwRequestInRequests, idx(Name), length(Method)
                                 | Method],
    snmp_value(OID).


resps(Name, Code) ->
    OID = ?wwwResponseOutEntry ++ [?wwwResponseOutResponses, idx(Name), Code],
    snmp_value(OID).


snmp_value(OID) ->
    {noError, 0, [Var]} = ct_snmp:get_values(cowboy_mib_test, [OID],
                                             snmp_mgr_agent),
    #varbind{oid = OID, value = Value} = Var,
    Value.
