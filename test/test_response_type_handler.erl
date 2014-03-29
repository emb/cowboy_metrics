-module(test_response_type_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([to_binary/2, to_iolist/2, to_other/2]).


init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.


content_types_provided(Req, State) ->
    {[
      {<<"text/binary+plain">>, to_binary}
     ,{<<"text/iolist+plain">>, to_iolist}
     ,{<<"text/other+plain">>, to_other}
     ], Req, State}.


to_binary(Req, State) ->
    {<<"hello world">>, Req, State}.


to_iolist(Req, State) ->
    {["hello", <<"world">>], Req, State}.


to_other(Req, State) ->
    {[{"hello", "world"}], Req, State}.

