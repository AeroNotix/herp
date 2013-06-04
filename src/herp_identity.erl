-module(herp_identity).
-define(REGION_URL, "https://region-b.geo-1.identity.hpcloudsvc.com:35357/v2.0/").

%% API
-export([login/3, login_conf/0]).

%% @doc Logs in and returns a Client for which we can use for futher
%% service calls.
%% @spec login(Username::string(), Password::string(), TenantID::string) ->
%%      {ok, Pid}
%% where
%%   Pid = pid()
login(Username, Password, TenantID) ->
	Body = create_auth_body(Username, Password, TenantID),
	{ok, {{_HTTP, Status, _Msg}, _Headers, Resp}} = httpc:request(post, {string:concat(?REGION_URL, "tokens"),
                                                                         ["accept", "application/json"],
                                                                         "application/json", Body}, [], []),
	Pid = case Status of
			  200 ->
				  supervisor:start_child({global, herp_sup}, [{jsx:decode(list_to_binary(Resp)), TenantID}]);
			  _Else ->
				  {error, Status}
		  end,
	Pid.

%% @doc login_conf uses the login details supplied in the sys.config
%% file and calls login/3 with those details.
%% @spec login_conf() -> {ok, Pid}
%% where
%%   Pid = pid()
login_conf() ->
    {ok, Username} = application:get_env(herp, username),
    {ok, Password} = application:get_env(herp, password),
    {ok, TenantID} = application:get_env(herp, tenant_id),
    login(Username, Password, TenantID).

%% @doc create_auth_body is a helper which interpolates the login
%% details with a JSON body.
%% @private
create_auth_body(Username, Password, TenantID) ->
	U = list_to_binary(Username),
	P = list_to_binary(Password),
	T = list_to_binary(TenantID),
	Body = [{<<"auth">>,
			 [{<<"passwordCredentials">>,
			   [{<<"username">>, U},
				{<<"password">>, P}]},
			  {<<"tenantId">>, T}]}],
	jsx:encode(Body).
