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

-module(herp_block).

%% Public API
-export([create_block/2, create_block/3, delete_block/2, list_volumes/1]).

%% @doc
%% create_block/2 will create a block storage device in the HPCloud
%% with the given size.
%% @spec create_block(ClientRef::ref(), Size::integer()) -> ok | {error, Reason}
create_block(ClientRef, Size) ->
	create_block(ClientRef, Size, []).

%% @doc
%% create_block/2 will create a block storage device in the HPCloud
%% with the given size as well as setting the options Metadata correctly.
%% @spec create_block(ClientRef::ref(), Size::integer(), Options::list()) -> ok | {error, Reason}
create_block(ClientRef, Size, Options) ->
	AllOptions = Options ++ [{<<"size">>, Size}],
	gen_server:call(herp_refreg:lookup(ClientRef),
					{create_block, jsx:encode([{<<"volume">>, AllOptions}])}, 30000).

%% @doc
%% delete_block/2 will delete a block storage device from the HPCloud.
%% @spec delete_block(ClientRef::ref(), ID::integer()) -> ok | {error, Reason}
delete_block(ClientRef, ID) ->
	gen_server:call(herp_refreg:lookup(ClientRef),
					{delete_block, ID}).

%% @doc
%% list_volumes/1 will list all the available volumes you have in your account.
%% @spec list_volumes(ClientRef::ref()) -> proplist()
list_volumes(ClientRef) ->
	gen_server:call(herp_refreg:lookup(ClientRef),
					list_volumes).
