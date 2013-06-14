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

-module(herp_client).
-behaviour(gen_server).

-define(OBJECT_URL,  "https://region-b.geo-1.objects.hpcloudsvc.com/v1.0/").
-define(REGION_URL,  "https://region-b.geo-1.identity.hpcloudsvc.com:35357/v2.0/").
-define(COMPUTE_URL, "https://az-1.region-a.geo-1.compute.hpcloudsvc.com/v1.1/").
-define(BLOCK_URL, "https://az-1.region-a.geo-1.compute.hpcloudsvc.com/v1.1/").
-define(CDN_URL, "https://region-b.geo-1.cdnmgmt.hpcloudsvc.com/v1.0/").
-define(SERVER, ?MODULE).

%% gen_server behaviour
-export([start_link/1,code_change/3,handle_call/3,
         init/1,handle_cast/2,handle_info/2,terminate/2]).

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

%% Currently we don't use this for anything special but we still need
%% to override it for gen_server behaviour.
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
    list_endpoint(URL, State);

%% @doc Creates a new container/directory in the object store.
handle_call({create_directory, Container, Options}, _From, State) when Container =/= "" ->
    URL = ?OBJECT_URL ++ State#client.tokenid ++ "/" ++ Container,
    Request = {URL, base_headers(State) ++ Options, "application/directory", <<"">>},
    Response = httpc:request(put, Request, [], []),
    {ok, {{_HTTP, Status, _Msg}, _Headers, _Resp}} = Response,
    case Status of
        201 ->
            {reply, ok, State};
        202 ->
            {reply, ok, State};
        Else ->
            {reply, {error, Else}, State}
    end;

%% @doc
%% handle_call/3 (create_file) will take a Container and some binary
%% term and put that data into the container with the associated
%% Filename.
%%
%% This function will also take care of end-to-end integrity by
%% hashing the input file via md5 and comparing the returning md5
%% value with that. We do this to ensure that the file is not
%% corrupted in-flight.
%%
%% @spec handle_call({create_file::atom(), Container::string(), FileContents::iolist(),
%% Filename::string(), Options::list()}, _From::pid(), State::list()) -> ok | {error, Reason}
handle_call({create_file, Container, FileContents, Filename, Options}, _From, State) ->
    ContentType = proplists:get_value("Content-Type", Options),
    URL = ?OBJECT_URL ++ State#client.tokenid ++ "/" ++ Container ++ "/" ++ Filename,
    Request = {URL, base_headers(State) ++ Options, ContentType, FileContents},
    Response = httpc:request(put, Request, [], []),
    {ok, {{_HTTP, Status, _Msg}, Headers, _Resp}} = Response,
    MD5 = proplists:get_value("ETag", Options),
    MD5Remote = proplists:get_value("ETag", Headers),
    case MD5 =:= MD5Remote of
        true ->
            case Status of
                201 ->
                    {reply, ok, State};
                _Else ->
                    {reply, {error, Status}, State}
            end;
        _Else ->
            {reply, {error, hash_mismatch}, State}
    end;

handle_call({copy_file, From, To, Headers}, _From, State) ->
    URL = ?OBJECT_URL ++ State#client.tokenid ++ "/" ++ To,
    AllHeaders = base_headers(State) ++ Headers ++ [{"X-Copy-From", From},
                                                    {"Content-length", "0"}],
    ContentType = "",
    Body = <<"">>,
    Request = {URL,  AllHeaders, ContentType, Body},
    Response = httpc:request(put, Request, [], []),
    {ok, {{_HTTP, Status, _Msg}, _Headers, _Resp}} = Response,
    case Status of
        201 ->
            {reply, ok, State};
        _Else ->
            {reply, {error,Status}, State}
    end;

handle_call({create_server, Body}, _From, State) ->
    URL = ?COMPUTE_URL ++ State#client.tokenid ++ "/servers",
    Request = {URL, base_headers(State), "application/json", Body},
    Response = httpc:request(post, Request, [], []),
    {ok, {{_HTTP, Status, _Msg}, _Headers, Resp}} = Response,
    case Status of
        202 ->
            {reply, ok, State};
        _Else ->
            Reply = herp_compute:error(Status, Resp),
            {reply, Reply, State}
    end;

handle_call({create_block, Body}, _From, State) ->
	URL = ?BLOCK_URL ++ State#client.tokenid ++ "/os-volumes",
	Request = {URL, base_headers(State), "application/json",
			   Body},
	Response = httpc:request(post, Request, [], []),
	{ok, {{_HTTP, Status, _Msg}, _Headers, Resp}} = Response,
	case Status of
		200 ->
			{reply, ok, State};
		_Else ->
			Reply = herp_block:error(Status, Resp),
			{reply, Reply, State}
	end;

handle_call({delete_block, ID}, _From, State) ->
	URL = ?BLOCK_URL ++ State#client.tokenid ++ "/os-volumes/" ++
		integer_to_list(ID),
    {Status, Resp} = generic_request(delete, State, URL),
	case Status of
		202 ->
			{reply, ok, State};
		_Else ->
			Reply = herp_block:error(Status, Resp),
			{reply, Reply, State}
	end;

handle_call(list_flavours, _From, State) ->
    URL = ?COMPUTE_URL ++ State#client.tokenid ++ "/flavors",
    list_endpoint(URL, State);

handle_call(list_images, _From, State) ->
    URL = ?COMPUTE_URL ++ State#client.tokenid ++ "/images",
    list_endpoint(URL, State);

handle_call(list_volumes, _From, State) ->
	URL = ?BLOCK_URL ++ State#client.tokenid ++ "/os-volumes",
	list_endpoint(URL, State);

handle_call(list_containers, _From, State) ->
	URL = ?CDN_URL ++ State#client.tokenid ++ "?format=json",
	list_endpoint(URL, State);

handle_call({enable_container, Container, TTL}, _From, State) ->
	URL = ?CDN_URL ++ State#client.tokenid ++ "/" ++ Container,
	Headers = [{"X-TTL", integer_to_list(TTL)}],
    Status = generic_request(post, State, URL, Headers),
    case Status of
		201 ->
			{reply, ok, State};
		202 ->
			{reply, ok, State};
		_Else ->
			{reply, {error, Status}, State}
	end;

handle_call({disable_container, Container}, _From, State) ->
    URL = ?CDN_URL ++ State#client.tokenid ++ "/" ++ Container,
    Headers = [{"X-CDN-Enabled", "False"}],
    Status = generic_request(post, State, URL, Headers),
    case Status of
        202 ->
            {reply, ok, State};
        _Else ->
            {reply, {error, Status}, State}
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
%% @spec quit(ClientRef::ref()) -> ok
quit(ClientRef) ->
    gen_server:cast(herp_refreg:lookup(ClientRef), {quit, ClientRef}).

base_headers(#client{access = Access}) ->
    [{"accept", "application/json"},
     {"X-Auth-Token", Access}].

%% @doc
%% list_endpoint/2 is a helper method to be used in all "listing"
%% endpoints. I.e. those which are just GET'ting an endpoint and
%% succeed on 200 and 203.
%%
%% @spec list_endpoint(URL::string(), State::state()) -> proplist()
list_endpoint(URL, State) ->
    Request = {URL, base_headers(State)},
    Response = httpc:request(get, Request, [], []),
    {ok, {{_HTTP, Status, _Msg}, _Headers, Resp}} = Response,
    JSONResp = list_to_binary(Resp),
    case Status of
        200 ->
            {reply, jsx:decode(JSONResp), State};
        203 ->
            {reply, jsx:decode(JSONResp), State};
        _Else ->
            {reply, {error, Status}, State}
    end.

%% @doc
%% generic_request/4 is a way of handling generic HTTP POST requests
%% which have no special need to set their body and the accompanying
%% Content-Type.
%% @spec generic_request(post, State::record(), URL::string(), Headers::list()) -> integer()
generic_request(post, State, URL, Headers) ->
    Request = {URL, base_headers(State) ++ Headers, "", <<"">>},
    Response = httpc:request(post, Request, [], []),
    {ok, {{_HTTP, Status, _Msg}, _Headers, _Resp}} = Response,
    Status.

%% @doc
%% generic_request/3 is a way of handling generic HTTP DELETE requests.
%% @spec generic_request(atom(), State::integer(), URL::string()) -> {Status::integer(), Resp::binary()}
generic_request(delete, State, URL) ->
    Request = {URL, base_headers(State)},
    Response = httpc:request(delete, Request, [], []),
    {ok, {{_HTTP, Status, _Msg}, _Headers, Resp}} = Response,
    {Status, Resp}.
