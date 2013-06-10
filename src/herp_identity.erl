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

-module(herp_identity).

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
    herp_client:new(Body, TenantID).

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
