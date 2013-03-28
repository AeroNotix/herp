-module(identity).
-compile(export_all).
-define(REGION_URL, "https://region-b.geo-1.identity.hpcloudsvc.com:35357/v2.0/").


login(Username, Password, TenantID) ->
	Body = create_auth_body(Username, Password, TenantID),
	io:format("~p~n", [Body]),
	httpc:request(post, {string:concat(?REGION_URL, "tokens"),
                         ["accept", "application/json"],
						 "application/json", Body}, [], []).

create_auth_body(Username, Password, TenantID) ->
    Body = "{\"auth\":{\"passwordCredentials\":{\"username\":\"~s\",\"password\":\"~s\"},\"tenantId\":\"~s\"}}",
    printf(Body, [Username, Password, TenantID]).

printf(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).
