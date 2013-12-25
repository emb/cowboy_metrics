%% Copyright (c) 2013-2015, Ed Mohmaed Barwani <ed.barwani@gmail.com>
%% All rights reserved.

%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are
%% met:

%% 1. Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.

%% 2. Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.

%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%% HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @doc SNMP related functions
-module(cowboy_metrics_snmp).

%% Potential Backend callbacks
-export([init/0, terminate/0]).
-export([increment_request/1]).

%% @doc Load cowboy mibs.
-spec init() -> ok | {error, term()}.
init() ->
    load_mibs().


%% @doc Unload cowboy mibs.
-spec terminate() -> ok | {error, term()}.
terminate() ->
    unload_mibs().


%% @doc Increment incoming cowboy requests
-spec increment_request(binary()) -> ok.
increment_request(_Method) ->
    snmp_generic:variable_inc({cowboyTotalRequests, volatile}, 1).


load_mibs() ->
    MIB = code:priv_dir(cowboy_metrics) ++ "/mibs/COWBOY-MIB",
    snmpa:load_mib(MIB).


unload_mibs() ->
    snmpa:unload_mib("COWBOY-MIB").
