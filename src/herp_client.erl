-module(herp_client).
-behaviour(gen_server).

-define(OBJECT_URL, "https://region-b.geo-1.objects.hpcloudsvc.com/v1.0/").

%% gen_server behaviour
-export([start/1,code_change/3,handle_call/3,init/1,handle_cast/2,handle_info/2,terminate/2]).

%% Client API
-export([list/1,list/2]).

-record(client, {access, tokenid, expires}).

init({LoginResp, TokenID}) ->
	ClientRec = extract_authtoken(LoginResp),
	ClientRec2 = ClientRec#client{tokenid=TokenID},
	{ok, ClientRec2}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({list, Container}, _From, State) ->
	io:format("~p ~p~n", [State, Container]),
	{reply, ok, State}.

%% We don't have any specific needs for these yet but we need to over-
%% ride them for the gen_server behaviour.
handle_cast(_Request, State) ->
    {noreply, State}.
handle_info(_Message, Library) ->
    {noreply, Library}.
terminate(_Reason, _Library) -> ok.

start(State) ->
	gen_server:start_link(?MODULE, State, []).

extract_authtoken(LoginResp) ->
	Access = proplists:get_value(<<"access">>, LoginResp),
	Token = proplists:get_value(<<"token">>, Access),
	AuthToken = proplists:get_value(<<"id">>, Token),
	Expires = proplists:get_value(<<"expires">>, Token),
	#client{access=binary_to_list(AuthToken), expires=Expires}.

%% Client-functions
%%
%% list_containers will list all the containers for your account,
%% list/1 will list all the base containers and list/2 will list all
%% the subcontainers for what you provide.
list(Pid) ->
    gen_server:call(Pid, {list, ""}).
list(Pid, Container) ->
    gen_server:call(Pid, {list, Container}).
