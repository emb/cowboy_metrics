[![Build Status](https://travis-ci.org/emb/cowboy_metrics.svg?branch=release/v0.1.0)](https://travis-ci.org/emb/cowboy_metrics)
# README

In long running cowboy application, sometimes log files are not enough
to monitor the behaviour of the application. SNMP is a good starting
point to introduce intstrumentation in cowboy. Hence this application.

Cowboy `onrequest` and `onresponse` hooks are used for this
instrumentation. See
[Cowboy Hooks](http://ninenines.eu/docs/en/cowboy/HEAD/guide/hooks/)
for details on the hooks.

## SNMP

This application implements the following tables from WWW-MIB defined
in [RFC-2594](https://tools.ietf.org/html/rfc2594):

* Service Information Table
* Protocol Summary Table
* Request **In** Table
* Response **Out** Table

## Usage

To use this application, you will need to configure and start erlang
snmp agent. Some help
[here](https://erlangcentral.org/wiki/index.php/SNMP_Quick_Start). After
that configure your cowboy application to use
`cowboy_metrics:on_requeqst/1` &
`cowboy_metrics:on_response/4`. Alternatively, you could use
`cowboy_metrics:start_http/5` instead of `cowboy:start_http/4`.

### Example

Starting a cowboy http server with index of 4.

```erlang
    %% Snippet
    Dispatch = cowboy_router:compile([{'_', [
                                             {"/", test_handler, []}
                                            ]}
                                     ]),
    {ok, _} = cowboy_metrics:start_http(4, http_handler, 100, [{port, 80}],
                                        [{env, [{dispatch, Dispatch}]}]),
    %% End snippet
```


## Thank you

* [SNMP Getting Started](https://erlangcentral.org/wiki/index.php/SNMP_Quick_Start)
* [snmp:config/0](http://www.erlang.org/doc/man/snmp.html#config-0)
* [Erlang os_mon source](https://github.com/erlang/otp/tree/maint/lib/os_mon)
