%%% @author Aaron France <aaron.l.france@gmail.com>

-module(herp_client).
-behaviour(gen_server).

-define(OBJECT_URL, "https://region-b.geo-1.objects.hpcloudsvc.com/v1.0/").
-define(REGION_URL, "https://region-b.geo-1.identity.hpcloudsvc.com:35357/v2.0/").
-define(SERVER, ?MODULE).

%% gen_server behaviour
-export([start_link/1,code_change/3,handle_call/3,init/1,handle_cast/2,handle_info/2,terminate/2]).

%% Client API
-export([new/2, quit/1]).

%% A client record encapsulates the three most important pieces
%% of information which needs to be stored about a client session.
-record(client, {access, tokenid, expires}).

%% @doc initializes a new gen_server Pid of our herp_client.
%% @spec init({LoginResp::string(), TokenID::string()}) -> {ok, Pid}
%% where
%%   Pid = pid()
init({Body, TokenID, Ref}) ->
	Request = {string:concat(?REGION_URL, "tokens"),
			   ["accept", "application/json"],
			   "application/json", Body},
	Response = httpc:request(post, Request, [], []),
	{ok, {{_HTTP, Status, _Msg}, _Headers, Resp}} = Response,
	case Status of
		200 ->
			LoginResp = jsx:decode(list_to_binary(Resp)),
			ClientRec = extract_authtoken(LoginResp),
			ClientRec2 = ClientRec#client{tokenid=TokenID},
			ok = herp_refreg:register(Ref, self()),
			{ok, ClientRec2};
		_Else ->
			{stop, Status}
	end.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% @doc Lists the containers on the objectstore/CDN service.
handle_call({list, Container}, _From, State) ->
	URL = case Container of
			  "" ->
				  string:concat(?OBJECT_URL, State#client.tokenid);
			  _Else ->
				  ?OBJECT_URL ++ State#client.tokenid ++ "/" ++ Container
		  end,
	Request = {URL, [{"accept", "application/json"},
					 {"X-Auth-Token", State#client.access}]},
	Response = httpc:request(get, Request, [], []),
	{ok, {{_HTTP, Status, _Msg}, _Headers, Resp}} = Response,
	case Status of
		200 ->
			{reply, jsx:decode(list_to_binary(Resp)), State};
		Else ->
			{reply, {error, Else}, State}
	end.

handle_cast({quit, ClientRef}, State) ->
	Reason = normal,
	herp_refreg:delete(ClientRef),
	{stop, Reason, State}.

handle_info(_Message, Library) ->
	{noreply, Library}.
terminate(_Reason, _Library) -> ok.

start_link(State) ->
	gen_server:start_link(?MODULE, State, []).

extract_authtoken(LoginResp) ->
	Access = proplists:get_value(<<"access">>, LoginResp),
	Token = proplists:get_value(<<"token">>, Access),
	AuthToken = proplists:get_value(<<"id">>, Token),
	Expires = proplists:get_value(<<"expires">>, Token),
	#client{access=binary_to_list(AuthToken), expires=Expires}.

%% @doc
%% new/2 will create a new client to the HPCloud service.
%% @spec new(Body::proplist(), TenantID::string()) -> ref()
new(Body, TenantID) ->
	Ref = make_ref(),
	{ok, _Pid} = herp_client_sup:start_child(Body, TenantID, Ref),
	Ref.

%% @doc
%% quit/1 will end a session
%% @spec end(ClientRef::ref())
quit(ClientRef) ->
	gen_server:cast(herp_refreg:lookup(ClientRef), {quit, ClientRef}).
