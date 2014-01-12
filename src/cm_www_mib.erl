%% Copyright (c) 2013-2015, Ed Mohamed Barwani <ed.barwani@gmail.com>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are
%% met:
%%
%% 1. Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%%
%% 2. Redistributions in binary form must reproduce the above
%% copyright notice, this list of conditions and the following
%% disclaimer in the documentation and/or other materials provided
%% with the distribution.
%%
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

%% @doc Implement WWW-MIB from RFC-2594
-module(cm_www_mib).

-include("cm_snmp.hrl").
-include("NETWORK-SERVICES-MIB.hrl").
-include("WWW-MIB.hrl").

%% API
-export([load/0, load/1]).
-export([unload/0, unload/1]).

-export([create_service_entry/1]).

-define(MIB, "WWW-MIB").

%% FIXME: Assuming that we are always using volatile databases for
%% these tables. Maybe this should be configurable in the future.
-define(SVC_DB, {wwwServiceTable, volatile}).

%% @doc load mibs to the default master agent.
-spec load() -> ok | {error, term()}.
load() ->
    load(snmp_master_agent).


-spec load(pid() | atom()) -> ok | {error, term()}.
load(Agent) ->
    Path = [code:priv_dir(cowboy_metrics), "mibs", ?MIB],
    snmpa:load_mib(Agent, filename:join(Path)).


%% @doc unload mib from the default agent.
-spec unload() -> ok | {error, term()}.
unload() ->
    unload(snmp_master_agent).


-spec unload(pid() | atom()) -> ok | {error, term()}.
unload(Agent) ->
    snmpa:unload_mib(Agent, ?MIB).


-spec create_service_entry(#service{}) -> true | false | {error, term()}.
create_service_entry(Svc = #service{index = Index}) ->
    %% Refer WwwServiceEntry in WWW-MIB.
    Row = {Index,
           Svc#service.description,
           Svc#service.contact,
           ?applTCPProtoID ++ [Svc#service.port],
           Svc#service.name,
           service_type(Svc#service.type),
           "00000000000", %% FIXME: StartTime (unknown)
           2,             %% FIXME: Operation Status (running)
           "00000000000"  %% FIXME: LastChange (unknown)
          },
    snmpa_mib_lib:table_cre_row(?SVC_DB, [Index], Row).



service_type(other) -> ?wwwServiceType_wwwOther; 
service_type(server) -> ?wwwServiceType_wwwServer;
service_type(client) -> ?wwwServiceType_wwwClient;
service_type(proxy) -> ?wwwServiceType_wwwProxy;
service_type(caching_proxy) -> ?wwwServiceType_wwwCachingProxy.
