-module(herp_client).
-behaviour(gen_server).

%% gen_server behaviour
-export([start_link/0, start/0,code_change/3,handle_call/3,init/1,handle_cast/2,handle_info/2,terminate/2]).

start_link() ->
	start().

init(_Args) ->
    {ok, []}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(_What, _From, State) ->
	{reply, ok, State}.

%% We don't have any specific needs for these yet but we need to over-
%% ride them for the gen_server behaviour.
handle_cast(_Request, State) ->
    {noreply, State}.
handle_info(_Message, Library) ->
    {noreply, Library}.
terminate(_Reason, _Library) -> ok.

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
