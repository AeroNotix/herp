-module(herp_object).

%% Public API
-export([list/1, list/2, create_directory/2]).

%% @doc
%% list/1 will list all the base containers which are available to
%% your account.
%% @spec list(Client::pid()) -> [string()]
list(ClientRef) ->
	gen_server:call(herp_refreg:lookup(ClientRef), {list, ""}).
%% @doc
%% list/2 will list all the subcontainers under the container name
%% which are available to your account.
%% @spec list(Client::pid(), Container::string()) -> [string()]
list(ClientRef, Container) ->
	gen_server:call(herp_refreg:lookup(ClientRef), {list, Container}).

%% @doc
%% create_directory/2 will create a new directory in the Object Store
%% you can create any level directory you need.
%% @spec create_directory(ClientRef::pid(), Container::string()) -> ok | {error, Reason}
create_directory(ClientRef, Container) ->
	gen_server:call(herp_refreg:lookup(ClientRef), {create_directory, Container}).
