-module(herp_compute).

-export([create_server/2, list_flavours/1, list_images/1, error/2]).

%% @doc
%% create_server/2 will provision a new server instance in the HPCloud.
%% @note you <em>must</em> provide:
%%    [{<<"flavorRef">>, ?FLAVOR_REF},
%%     {<<"imageRef">>, ?IMAGE_REF},
%%     {<<"name">>, string()}]
%% @spec create_server(ClientRef::ref(), ServerProp::proplist()) -> ok | {error, {Field::atom(), Reason}}
create_server(ClientRef, ServerProp) when is_list(ServerProp) ->
    Name = proplists:get_value(<<"name">>, ServerProp),
    Flavor = proplists:get_value(<<"flavorRef">>, ServerProp),
    ImageRef = proplists:get_value(<<"imageRef">>, ServerProp),
    case verify_compute_request([Name, Flavor, ImageRef]) of
        ok ->
            S = [{<<"server">>, ServerProp}],
            ServerEncoded = jsx:encode(S),
            gen_server:call(herp_refreg:lookup(ClientRef), {create_server, ServerEncoded});
        {error, Field} ->
            {error, {Field, missing}}
    end.

list_flavours(ClientRef) ->
    list_endpoint(ClientRef, list_flavours).

list_images(ClientRef) ->
    list_endpoint(ClientRef, list_images).

verify_compute_request([]) ->
    ok;
verify_compute_request([H|T]) ->
    case H of
        undefined ->
            {error, H};
        _Else ->
            verify_compute_request(T)
    end.

error(Status, Body) ->
    case Status of
        400 ->
            extract_error_field(<<"badRequest">> Body)
        _Else ->
            exit(self(), "Unhandled api error")
    end.

extract_error_field(Field, Body) ->
    JSONBody = jsx:decode(list_to_binary(Body)),
    BadRequest = proplists:get_value(Field, JSONBody),
    {error, proplists:get_value(<<"message">>, BadRequest)};

list_endpoint(ClientRef, Which) ->
    gen_server:call(herp_refreg:lookup(ClientRef), Which).
