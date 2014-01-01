-module(request_counter_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").

-include_lib("snmp/include/snmp_types.hrl").
-include_lib("../include/COWBOY-MIB.hrl").

%% ct callbacks
-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).

%% tests
-export([
         %% Generic request generator and counter.
         request_counter_test/1,

         %% SNMP specific tests.
         snmp_total_init_test/1
	]).


suite() ->
    %% require certain configuration for snmp tests.
    [
     {require, snmp_mgr_agent, snmp},
     {require, snmp_app, snmp_app}
    ].


all() ->
    [
     {group, snmp}
    ].


groups() ->
    [
     {snmp, [], [request_counter_test, snmp_total_init_test]}
    ].


start_cowboy_app(Config) ->
    {ok, _} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([
				      {'_', [
					     {"/", test_handler, []}
					    ]}
				     ]),
    {ok, _} = cowboy:start_http(test_http, 100, [{port, 0}],
				[
				 {env, [{dispatch, Dispatch}]},
				 {onrequest, fun cowboy_metrics:on_request/1}
				]),
    Port = ranch:get_port(test_http),
    {ok, [{http_port, Port} | Config]}.


init_per_suite(Config) ->
    {ok, Config2} = start_cowboy_app(Config),
    ok = application:ensure_started(ibrowse),
    Config2.


end_per_suite(Config) ->
    application:stop(ibrowse),
    application:stop(cowoby),
    Config.


init_per_group(snmp, Config) ->
    %% Start a convinient snmp manager and agent.
    ok = ct_snmp:start(Config, snmp_mgr_agent, snmp_app),
    {ok, _} = application:ensure_all_started(cowboy_metrics),
    
    [{count_getter, fun snmp_count_getter/1} | Config].


end_per_group(snmp, Config) ->
    application:stop(cowboy_metrics),
    ct_snmp:stop(Config),
    
    Config.


uri(Config) ->
    Port = ?config(http_port, Config),
    lists:flatten(io_lib:format("http://localhost:~p/", [Port])).


get_snmp_counter(OID) ->
    {noError, 0, [Var]} = ct_snmp:get_values(cowboy_mib_test, [OID],
                                             snmp_mgr_agent),

    #varbind{oid = OID, variabletype = 'Counter32', value = Value} = Var,

    Value.


method(Method) when is_atom(Method) ->
    string:to_upper(atom_to_list(Method)).


snmp_count_getter(Method) when is_atom(Method) ->
    snmp_count_getter(method(Method));
snmp_count_getter("TOTAL") ->
    get_snmp_counter(?totalRequests_instance);
snmp_count_getter(Method) ->
    OID = ?requestEntry ++ [?requestMethodCount] ++ [length(Method) | Method],
    get_snmp_counter(OID).


methods() ->
    [get, head, post, put, patch, delete, options].


request_counter_test(Config) ->
    true = proper:quickcheck(prop_generate_requests(Config)),
    %% quickcheck does a 100 by default.
    Getter = ?config(count_getter, Config),
    100 = Getter("TOTAL").


prop_generate_requests(Config) ->
    Getter = ?config(count_getter, Config),
    ?FORALL({Method, Path}, 
            {oneof(methods()), oneof(["", "non-exisiting-path"])}, 
            begin
                Count = Getter(Method),
                Uri = uri(Config) ++ Path,
                {ok, _, _, _} = ibrowse:send_req(Uri, [], Method),
                NewCount = Getter(Method),

                ?WHENFAIL(
                   ct:pal(error, "did not increment counter, old: ~p new: ~p",
                          [Count, NewCount]),
                   NewCount =:= Count + 1)
            end).


snmp_total_init_test(doc) ->
    ["Ensure the total request counter gets initialized during an error"];
snmp_total_init_test(_Config) ->
    %% Kill the gen_server this should erace its state along with the
    %% total_request counter.
    exit(whereis(cowboy_metrics_server), kill),

    %% Just wait a little for the process to be spawned.
    ct:sleep({seconds, 2}),

    %% Now query SNMP which should return a total of 100 from the previous test.
    100 = snmp_count_getter("TOTAL").
    
