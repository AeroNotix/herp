%%% @author Aaron France <aaron.l.france@gmail.com>
%%% Copyright (c) 2013, Aaron France
%%% All rights reserved.

%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:

%%%     * Redistributions of source code must retain the above
%%%     copyright notice, this list of conditions and the following
%%%     disclaimer.

%%%     * Redistributions in binary form must reproduce the above
%%%       copyright notice, this list of conditions and the following
%%%       disclaimer in the documentation and/or other materials
%%%       provided with the distribution.

%%%     * Neither the name of Aaron France nor the names of its
%%%       contributors may be used to endorse or promote products
%%%       derived from this software without specific prior written
%%%       permission.

%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
%%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
%%% EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
%%% TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
%%% THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.

-module(herp_compute).

-export([create_server/2, list_flavours/1, list_images/1, error/2, flavour/1]).

%% @doc
%% create_server/2 will provision a new server instance in the HPCloud.
%% you <em>must</em> provide:
%% ```
%%    [{<<"flavorRef">>, ?FLAVOR_REF},
%%     {<<"imageRef">>, ?IMAGE_REF},
%%     {<<"name">>, string()}]
%% '''
%% @spec create_server(ClientRef::ref(), ServerProp::proplist()) -> ok | {error, {Field::atom(), Reason}}
create_server(ClientRef, ServerProp) when is_list(ServerProp) ->
    Name = proplists:get_value(<<"name">>, ServerProp),
    Flavor = proplists:get_value(<<"flavorRef">>, ServerProp),
    ImageRef = proplists:get_value(<<"imageRef">>, ServerProp),
    case herp_generic:verify_plist([Name, Flavor, ImageRef]) of
        ok ->
            S = [{<<"server">>, ServerProp}],
            ServerEncoded = jsx:encode(S),
            gen_server:call(herp_refreg:lookup(ClientRef), {create_server, ServerEncoded}, 30000);
        {error, Field} ->
            {error, {Field, missing}}
    end.

%% @doc
%% delete_server/2 will take a ServerID and schedule that server for
%% termination.
%% @spec delete_server(ClientRef::ref(), ServerID::binary()) -> ok | {error, Reason}
delete_server(ClientRef, ServerID) ->
    gen_server:call(herp_refreg:lookup(ClientRef), {delete_server, ServerID}).

%% @doc
%% list_flavours/1 returns a proplist with all the available flavours
%% in the HPCloud. You shouldn't really need to use this as the list
%% never really changes and we provide flavour/1 which holds all you
%% need.
%% @spec list_flavours(ClientRef::ref()) -> proplist()
list_flavours(ClientRef) ->
    list_endpoint(ClientRef, list_flavours).

%% @doc
%% list_images/1 returns a proplist with all the available images in
%% the HPCloud. The images are fully listed and can be used to
%% provision new servers.
%% @spec list_images(ClientRef::ref()) -> proplist()
list_images(ClientRef) ->
    list_endpoint(ClientRef, list_images).

%% @doc
%% The compute API has a very clear outline for the different kinds of
%% errors which it handles. They are all in the form:
%% ```
%% {
%%      $REASON: {
%%          "message": $MESSAGE,
%%          "code"   : $CODE,
%%          "detail" : $DETAIL
%%      }
%% }
%% '''
%%
%% This function will retreive the specific body for each message and
%% return the correct message for it.
%% @spec error(Status::ref(), Body::proplist()) -> Exit | {error, Reason}
error(Status, Body) ->
    case Status of
        400 ->
            extract_error_field(<<"badRequest">>, Body);
        _Else ->
            exit(self(), normal)
    end.

%% @doc
%% Extracts the message field from a Compute API error message.
extract_error_field(Field, Body) ->
    JSONBody = jsx:decode(list_to_binary(Body)),
    BadRequest = proplists:get_value(Field, JSONBody),
    {error, proplists:get_value(<<"message">>, BadRequest)}.

%% @doc
%% Helper method to simplify calling list_* endpoints.
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
