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

%% @doc Entry module to this application.
-module(cowboy_metrics).

-export([on_request/1, on_response/4]).

%% @doc Use this function as the `cowboy:onrequest_fun()'. It will be
%% used collect metrics for all cowboy incoming requests.
-spec on_request(cowboy_req:req()) -> cowboy_req:req().
on_request(Req) ->
    {Method, Req1} = cowboy_req:method(Req),
    gen_server:cast(cowboy_metrics_server, {request, Method}),
    Req1.


%% @doc Use this function as the `cowboy:onresponse_fun()'. Used for
%% collecting response metrices.
-spec on_response(cowboy:http_status(), cowboy:http_headers(),
                  iodata(), cowboy_req:req()) -> cowboy_req:req().
on_response(Status, _, Body, Req) ->
    Size = byte_size(Body),
    gen_server:cast(cowboy_metrics_server, {response, Status, Size}),
    Req.
