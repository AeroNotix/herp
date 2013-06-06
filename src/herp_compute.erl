-module(herp_compute).

-export([create_server/2, list_flavours/1, list_images/1, error/2, flavour/1]).

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
            gen_server:call(herp_refreg:lookup(ClientRef), {create_server, ServerEncoded}, 30000);
        {error, Field} ->
            {error, {Field, missing}}
    end.

delete_server(ClientRef, ServerID) ->
    gen_server:call(herp_refreg:lookup(ClientRef), {delete_server, ServerID}).

%% @doc
%% list_flavours/1 returns a proplist with all the available flavours
%% in the HPCloud. You shouldn't really need to use this as the list
%% never really changes and we provide flavour/1 which holds all you
%% need.
list_flavours(ClientRef) ->
    list_endpoint(ClientRef, list_flavours).

%% @doc
%% list_images/1 returns a proplist with all the available images in
%% the HPCloud. The images are fully listed and can be used to
%% provision new servers.
list_images(ClientRef) ->
    list_endpoint(ClientRef, list_images).

%% @doc
%% verify_compute_request/1 will take a list of values and if any are
%% undefined return an error. We can use this to simplify checking
%% input proplists to functions like create_server/2
verify_compute_request([]) ->
    ok;
verify_compute_request([H|T]) ->
    case H of
        undefined ->
            {error, H};
        _Else ->
            verify_compute_request(T)
    end.

%% @doc
%% The compute API has a very clear outline for the different kinds of
%% errors which it handles. They are all in the form:
%%
%% {
%%      $REASON: {
%%          "message": $MESSAGE,
%%          "code"   : $CODE,
%%          "detail" : $DETAIL
%%      }
%% }
%%
%% @spec error(Status::ref(), Body::proplist()) -> Exit | {error, Reason}
error(Status, Body) ->
    case Status of
        400 ->
            extract_error_field(<<"badRequest">>, Body);
        _Else ->
            exit(self(), normal)
    end.

extract_error_field(Field, Body) ->
    JSONBody = jsx:decode(list_to_binary(Body)),
    BadRequest = proplists:get_value(Field, JSONBody),
    {error, proplists:get_value(<<"message">>, BadRequest)}.

list_endpoint(ClientRef, Which) ->
    gen_server:call(herp_refreg:lookup(ClientRef), Which).

%% @doc
%% flavour/1 will return the appropriate term for the flavorRef.
%% @spec flavour(atom()) -> binary()
flavour(xsmall) ->
    <<"100">>;
flavour(small) ->
    <<"101">>;
flavour(medium) ->
    <<"102">>;
flavour(large) ->
    <<"103">>;
flavour(xlarge) ->
    <<"104">>;
flavour(largex2) ->
    <<"105">>.
