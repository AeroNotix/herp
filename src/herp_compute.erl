-module(herp_compute).

-export([create_server/2]).

create_server(ClientRef, ServerProp) when is_list(ServerProp) ->
    S = [{<<"server">>, ServerProp}],
    io:format("~p~n", [S]),
    ServerEncoded = jsx:encode(S),
    gen_server:call(herp_refreg:lookup(ClientRef), {create_server, ServerEncoded}).
