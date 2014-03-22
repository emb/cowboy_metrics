-module(test_handler).

-export([init/3, handle/2, terminate/3]).


init(_Transport, Req, _Opts) ->
    {ok, Req, undefined}.


handle(Req, State) ->
    {Code, Req1} = cowboy_req:qs_val(<<"code">>, Req, <<"204">>),
    {Body, Req2} = cowboy_req:qs_val(<<"body">>, Req1, <<"">>),

    Headers = [{<<"content-type">>, <<"text/plain">>}],
    {ok, Req3} = cowboy_req:reply(binary_to_integer(Code), Headers , Body,
                                  Req2),
    {ok, Req3, State}.


terminate(_Reason, _Req, _State) ->
    ok.
