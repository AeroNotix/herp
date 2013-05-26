-module(herp_client).
-behaviour(gen_server).

%% gen_server behaviour
-export([start/1,code_change/3,handle_call/3,init/1,handle_cast/2,handle_info/2,terminate/2]).

init(LoginResp) ->
	{ok, [LoginResp, extract_authtoken(LoginResp)]}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({hey, Value}, _From, State) ->
	io:format("~p~n", [State]),
	{reply, ok, State}.

%% We don't have any specific needs for these yet but we need to over-
%% ride them for the gen_server behaviour.
handle_cast(_Request, State) ->
    {noreply, State}.
handle_info(_Message, Library) ->
    {noreply, Library}.
terminate(_Reason, _Library) -> ok.

start(State) ->
	gen_server:start_link(?MODULE, [State], []).
extract_authtoken(LoginResp) ->
	Access = proplists:get_value(<<"access">>, LoginResp),
	Token = proplists:get_value(<<"token">>, Access),
	AuthToken = proplists:get_value(<<"id">>, Token),
	Expires = proplists:get_value(<<"expires">>, Token),
	{Access, AuthToken, Expires}.
