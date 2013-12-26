-module(request_counter_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("snmp/include/snmp_types.hrl").
-include_lib("../include/COWBOY-MIB.hrl").

-export([
         all/0,
	 groups/0,
         init_per_suite/1,  end_per_suite/1
        ]).


-export([
	 send_success_request/1,
	 send_404_request/1,
	 check_snmp_counter/1
	]).

-define(NUM_REQUESTS, 20).

all() ->
    [
     %% FIXME: Find a better way to send parallel requests.
     %% sending requests is not really a test it is the counter that matters.
     {group, send_requests},
     check_snmp_counter
    ].


groups() ->
    [
     {send_requests,
      [parallel, {repeat, ?NUM_REQUESTS/2}],
      [send_success_request, send_404_request]}
    ].


setup_db_dir(Type, Config) ->
    PrivDir = ?config(priv_dir, Config),
    DbDir = PrivDir ++ Type ++ "/db/",
    ok = filelib:ensure_dir(DbDir),
    DbDir.


setup_snmp_app(Config) ->
    DataDir = ?config(data_dir, Config),
    Agent = [{config, [{dir, DataDir ++ "snmp/agent/conf/"}]},
             {db_dir, setup_db_dir("agent", Config)}],
    Manager = [{priority, normal},
               {versions, [v2]},
               {config, [{dir, DataDir ++ "/snmp/manager/conf/"},
                         {db_dir, setup_db_dir("manager", Config)}]},
               {mibs, []}
              ],

    application:stop(snmp),
    application:set_env(snmp, agent, Agent),
    application:set_env(snmp, manager, Manager),
    {application:start(snmp), Config}.


setup_cowboy_app(Config) ->
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
    {ok, Config1} = setup_snmp_app(Config),
    {ok, Config2} = setup_cowboy_app(Config1),
    ok = application:ensure_started(cowboy_metrics),
    ok = application:ensure_started(ibrowse),
    Config2.


end_per_suite(Config) ->
    application:stop(ibrowse),
    application:stop(cowboy_metrics),
    application:stop(cowoby),
    application:stop(snmp),
    Config.


send_success_request(Config) ->
    {ok, "200", _, "TEST HANDLER"} = ibrowse:send_req(uri(Config), [], get).


send_404_request(Config) ->
    Uri = uri(Config) ++ "non-exsisting-path",
    {ok, "404", _, _} = ibrowse:send_req(Uri, [], get).


uri(Config) ->
    Port = ?config(http_port, Config),
    lists:flatten(io_lib:format("http://localhost:~p/", [Port])).


check_snmp_counter(_Config) ->
    {ok, SnmpReply, _} = snmpm:sync_get(cowgirl, "cowboy agent",
			   [?totalRequests_instance]),

    ct:pal("snmp reply: ~p", [SnmpReply]),
    
    {noError, _, [Variable]} = SnmpReply,
    20 = Variable#varbind.value.

	

