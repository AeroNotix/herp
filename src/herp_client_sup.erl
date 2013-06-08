-module(herp_client_sup).

-behaviour(supervisor).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         start_link/0,
         start_child/3
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
         init/1
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Body, TenantID, Ref) ->
	supervisor:start_child(?SERVER, [{Body, TenantID, Ref}]).

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init/1
%%
%% herp_client_sup is a simple_one_for_one supervisor.
%%
%% The client which it is supervising is the real meat and potatoes of
%% application. It's the herp_client process. We have a 5k/d max
%% restart allowance.
%%--------------------------------------------------------------------
init([]) ->
    ClientWorkers = {herp_client, {herp_client, start_link, []},
                     transient, 5000, worker, [herp_client]},
    
    Children = [ClientWorkers],
    RestartStrategy = {simple_one_for_one, 5000, 26400},
    {ok, {RestartStrategy, Children}}.