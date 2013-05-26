-module(identity).
-compile(export_all).
-define(REGION_URL, "https://region-b.geo-1.identity.hpcloudsvc.com:35357/v2.0/").


login(Username, Password, TenantID) ->
	Body = string:join(["{\"auth\":{\"passwordCredentials\":{",
						"\"username\":\"", Username,
						"\",\"password\":\"", Password, "\"},"
						"\"tenantId\":\"", TenantID, "\"}}"], ""),
	io:format("~p~n", [Body]),
	httpc:request(post, {string:concat(?REGION_URL, "tokens"), ["accept", "application/json"],
						 "application/json", Body}, [], []).
