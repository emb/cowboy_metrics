# Introduce some instrumentation to a cowboy http server

TODO: Introduction

# Playing with SNMP agent

Just a quick play with the cowboy mib.

## Requirements

* A Linux flavour. This was never tested on any other platform.
* snmpwalk

    $ yum install net-snmp-utils # fedora

or

    $ aptitude install snmp # debian

* OTP Mibs & COWBOY mib to be in a directory where `snmpwalk` have
  access to. Refer to the `-M` option of `snmpwalk`.

You may find the OTP mibs in somwhere close `lib/erlang/snmp*/mibs`
and the cowboy mib `mibs/COWBOY-MIB.mib`

## Playing

Here is an exampl erlang shell. The crazy path
`../cowboy_metrics/ebin` was a hacky way to trick `code:priv_dir/1`.

For any reference regarding the snmp agent configuration refer to
`snmp/agent.config`.

**erlang shell**

    $ erl -sname foo -config snmp/agent -pa ../cowboy_metrics/ebin
    Erlang R16B03 (erts-5.10.4) [source] [64-bit] [async-threads:10] [kernel-poll:false]
    
    Eshell V5.10.4  (abort with ^G)
    (foo@turkish)1> application:start(snmp).
    ok

**snmpwaly** At this point we should be able to see some generic snmp data being
set.

    $ mkdir snmp/agent/db # Required for the agent to start.
    $ snmpwalk -m all -c public -v2c localhost:4000
    RFC1213-MIB::sysDescr.0 = STRING: "Erlang SNMP agent"
    RFC1213-MIB::sysObjectID.0 = OID: joint-iso-ccitt.46.1.4.1.193.19
    RFC1213-MIB::sysUpTime.0 = Timeticks: (37319) 0:06:13.19
    RFC1213-MIB::sysContact.0 = STRING: "cowboy@test.com"
    RFC1213-MIB::sysName.0 = STRING: "cowboy's test agent"
    RFC1213-MIB::sysLocation.0 = STRING: "erlang"
    ...

**erlang shell** Let us load the cowboy mib.

    (foo@turkish)2> cowboy_metrics_snmp:init().
    ok
    (foo@turkish)3> snmpa:which_mibs().
    [{'SNMP-MPD-MIB',"/usr/lib/erlang/lib/snmp-4.25/priv/mibs/SNMP-MPD-MIB.bin"},
     {'SNMPv2-MIB',"/usr/lib/erlang/lib/snmp-4.25/priv/mibs/SNMPv2-MIB.bin"},
     {'COWBOY-MIB',"../cowboy_metrics/priv/mibs/COWBOY-MIB.bin"},
     {'SNMP-FRAMEWORK-MIB',"/usr/lib/erlang/lib/snmp-4.25/priv/mibs/SNMP-FRAMEWORK-MIB.bin"}]

**snmpwalk** Try OTP as on OID

    $ snmpwalk -m all -c public -v2c localhost:4000 otp
    COWBOY-MIB::cowboyTotalRequests.0 = Counter32: 0

**erlang shell** Increase the counter

    (foo@turkish)4> cowboy_metrics_snmp:increment_request(<<"GET">>).
    ok
    (foo@turkish)5> cowboy_metrics_snmp:increment_request(<<"GET">>).
    ok

**snmpwalk** Did it increase?

    $ snmpwalk -m all -c public -v2c localhost:4000 otp
    COWBOY-MIB::cowboyTotalRequests.0 = Counter32: 2
