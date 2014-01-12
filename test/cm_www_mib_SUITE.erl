-module(cm_www_mib_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("snmp/include/snmp_types.hrl").
-include_lib("../include/WWW-MIB.hrl").
-include_lib("../include/NETWORK-SERVICES-MIB.hrl").
-include_lib("../include/cm_snmp.hrl").

%% ct callbacks.
-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1]).

%% tests
-export([
         create_svc_entry/1
        ]).


suite() ->
    %% SNMP Config
    [
     {require, snmp_mgr_agent, snmp},
     {require, snmp_app, snmp_app}
    ].


all() ->
    [
     {group, serviceTable}
    ].


groups() ->
    [
     {serviceTable, [], [create_svc_entry]}
    ].


init_per_suite(Config) ->
    ok = ct_snmp:start(Config, snmp_mgr_agent, snmp_app),
    ok = cm_www_mib:load(),
    Config.


end_per_suite(Config) ->
    ok = cm_www_mib:unload(),
    ct_snmp:stop(Config),
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
    Svc = #service{index = 4,
                   description = "create_svc_entry test",
                   contact = "cm_www_mib_SUITE",
                   name = "test@cowboy_metrics",
                   port = 8080,
                   type = proxy},

    true = cm_www_mib:create_service_entry(Svc),

    {noError, 0, Variables} = ct_snmp:get_values(cowboy_mib_test,
                                                 ?serviceEntries(
                                                    Svc#service.index),
                                                 snmp_mgr_agent),

    [Desc, Contact, Protocol, Name, Type] = Variables,

    check_value(Svc#service.description, Desc),
    check_value(Svc#service.contact, Contact),

    ProtoID = ?applTCPProtoID ++ [Svc#service.port],
    check_value(ProtoID, Protocol),
    
    check_value(Svc#service.name, Name),
    check_value(?wwwServiceType_wwwProxy, Type).


check_value(Expect, #varbind{oid = OID, value = Value}) ->
    ct:log(info, "varbind -> OID: ~p expected: ~p value: ~p",
           [OID, Expect, Value]),
    Expect = Value.
