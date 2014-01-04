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
         response_counter_test/1,
         response_size_test/1
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
     {snmp, [], [request_counter_test,
                 response_counter_test,
                 response_size_test
                ]}
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
                                 {onrequest, fun cowboy_metrics:on_request/1},
                                 {onresponse, fun cowboy_metrics:on_response/4}
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


snmp_count_getter(total_requests) ->
    get_snmp_counter(?totalRequests_instance);
snmp_count_getter(total_responses) ->
    get_snmp_counter(?totalResponses_instance);
snmp_count_getter(total_response_size) ->
    get_snmp_counter(?totalResponseSize_instance);
snmp_count_getter({request, Method}) when is_atom(Method) ->
    snmp_count_getter({request, method(Method)});
snmp_count_getter({request, Method}) ->
    OID = ?requestEntry ++ [?requestMethodCount] ++ [length(Method) | Method],
    get_snmp_counter(OID);
snmp_count_getter({response, [Class|_]}) ->
    Code = [Class, $x, $x],
    C_OID = ?responseEntry ++ [?responseCodeCount] ++ [length(Code) | Code],
    S_OID = ?responseEntry ++ [?responseCodeSize] ++ [length(Code) | Code],
    {get_snmp_counter(C_OID), get_snmp_counter(S_OID)}.


methods() ->
    [get, head, post, put, patch, delete, options].


codes() ->
    [
     %% Success Coodes
     "200", "201", "204",
     %% Redirection
     "300", "301", "302", "304",
     %% Bad Request
     "400", "401", "403", "404", "405", "415",
     %% Server Error
     "500", "503"
    ].


request_counter_test(Config) ->
    true = proper:quickcheck(prop_request_counter_test(Config)).

prop_request_counter_test(Config) ->
    Getter = ?config(count_getter, Config),
    ErrorWhenFail = "request counter did not increment"
        "~n  Total Count,   old: ~p\tnew: ~p"
        "~n  Method Count,  old: ~p\tnew: ~p~n",
    ?FORALL(Method, oneof(methods()),
            begin
                OldTotal = Getter(total_requests),
                Old = Getter({request, Method}),

                Uri = uri(Config),
                {ok, _, _, _} = ibrowse:send_req(Uri, [], Method),

                NewTotal = Getter(total_requests),
                New = Getter({request, Method}),

                Counts = [OldTotal, NewTotal, Old, New],
                ?WHENFAIL(ct:pal(error, ErrorWhenFail, Counts),
                          New =:= Old + 1 andalso NewTotal =:= OldTotal + 1)
            end).


response_counter_test(Config) ->
    true = proper:quickcheck(prop_response_counter_test(Config)).


prop_response_counter_test(Config) ->
    Getter = ?config(count_getter, Config),
    ErrorWhenFail = "response counter did not increment"
        "~n  Total Count,      old: ~p\tnew: ~p"
        "~n  Code Class Count, old: ~p\tnew: ~p~n",
    ?FORALL({Method, Code}, {oneof(methods()), oneof(codes())},
            begin
                OldTotal = Getter(total_responses),
                {Old, _} = Getter({response, Code}),

                Uri = uri(Config) ++ "?code=" ++ Code,
                {ok, Code, _, _} = ibrowse:send_req(Uri, [], Method),

                NewTotal = Getter(total_responses),
                {New, _} = Getter({response, Code}),

                Counts = [OldTotal, NewTotal, Old, New],
                ?WHENFAIL(ct:pal(error, ErrorWhenFail, Counts),
                          New =:= Old + 1 andalso NewTotal =:= OldTotal + 1)
            end).


response_size_test(Config) ->
    true = proper:quickcheck(prop_response_size_test(Config)).


prop_response_size_test(Config) ->
    Getter = ?config(count_getter, Config),
    ErrorWhenFail = "response size did not increment"
        "~n  Total Size,      old: ~p\tnew: ~p"
        "~n  Code Class Size, old: ~p\tnew: ~p~n",
    ?FORALL({Method, Code, Body},
            {oneof([head, get, put, post]),
             oneof(["200", "201"]),
             oneof(["hello", "hello_world", "anotherstring"])},
            begin
                OldTotal = Getter(total_response_size),
                {_, Old} = Getter({response, Code}),


                Uri = uri(Config) ++ "?code=" ++ Code ++ "&body=" ++ Body,
                {ok, Code, _, RetBody} =  ibrowse:send_req(Uri, [], Method),
                Len = length(RetBody),

                NewTotal = Getter(total_response_size),
                {_, New} = Getter({response, Code}),

                Count = [OldTotal, NewTotal, Old, New],
                ?WHENFAIL(ct:pal(error, ErrorWhenFail, Count),
                          New =:= Old + Len andalso NewTotal =:= OldTotal + Len)
            end).


