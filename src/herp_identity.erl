-module(herp_identity).
-compile(export_all).
-define(REGION_URL, "https://region-b.geo-1.identity.hpcloudsvc.com:35357/v2.0/").


login(Username, Password, TenantID) ->
	Body = create_auth_body(Username, Password, TenantID),
	{ok, {{_HTTP, Status, _Msg}, Headers, Resp}} = httpc:request(post, {string:concat(?REGION_URL, "tokens"),
																["accept", "application/json"],
																"application/json", Body}, [], []),
	case Status of
		200 ->
			jsx:decode(list_to_binary(Resp));
		_Else ->
			{error, Status}
	end.
			

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
