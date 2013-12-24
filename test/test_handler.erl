-module(test_handler).

-export([init/3]).
-export([content_types_provided/2, to_text/2]).


init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.


content_types_provided(Req, State) ->
    {[
      {<<"text/plain">>, to_text}
     ], Req, State}.


to_text(Req, State) ->
    {<<"TEST HANDLER">>, Req, State}.
