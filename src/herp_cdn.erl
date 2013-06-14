%%% @author Aaron France <aaron.l.france@gmail.com>
%%% Copyright (c) 2013, Aaron France
%%% All rights reserved.

%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:

%%%     * Redistributions of source code must retain the above
%%%     copyright notice, this list of conditions and the following
%%%     disclaimer.

%%%     * Redistributions in binary form must reproduce the above
%%%       copyright notice, this list of conditions and the following
%%%       disclaimer in the documentation and/or other materials
%%%       provided with the distribution.

%%%     * Neither the name of Aaron France nor the names of its
%%%       contributors may be used to endorse or promote products
%%%       derived from this software without specific prior written
%%%       permission.

%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
%%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
%%% EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
%%% TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
%%% THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.

-module(herp_cdn).
-export([list_containers/1, enable_container/2, enable_container/3, disable_container/2]).

%% @doc
%% list_containers/1 will list the CDN-enabled containers available to
%% your HPCloud account.
%% @spec list_containers(ClientRef::ref()) -> proplist() | {error, Reason}
list_containers(ClientRef) ->
    gen_server:call(herp_refreg:lookup(ClientRef), list_containers).

%% @doc
%% enable_container/2 will CDN-enable a container.
%% specified amount.
%% @spec enable_container(ClientRef::ref(), Container::string()) -> ok | {error, Reason}
enable_container(ClientRef, Container) ->
    enable_container(ClientRef, Container, 86400).

%% @doc
%% enable_container/3 will CDN-enable a container and set it's TTL to the
%% specified amount.
%% @spec enable_container(ClientRef::ref(), Container::string(), TTL::integer()) -> ok | {error, Reason}
enable_container(ClientRef, Container, TTL) ->
    gen_server:call(herp_refreg:lookup(ClientRef), {enable_container, Container, TTL}, 30000).

%% @doc
%% disable_container/2 will disable a CDN-enabled container.
%% @spec disable_container(ClientRef::ref(), Container::string()) -> ok | {eror, Reason}
disable_container(ClientRef, Container) ->
    gen_server:call(herp_refreg:lookup(ClientRef), {disable_container, Container}, 30000).
