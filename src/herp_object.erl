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

-module(herp_object).

%% Public API
-export([list/1, list/2, create_directory/2, create_directory/3,
         upload_file/3, upload_file/4, upload_file/5, copy_file/3,
         copy_file/4]).

%% @doc
%% list/1 will list all the base containers which are available to
%% your account.
%% @spec list(Client::pid()) -> [string()]
list(ClientRef) ->
	gen_server:call(herp_refreg:lookup(ClientRef), {list, ""}).

%% @doc
%% list/2 will list all the subcontainers under the container name
%% which are available to your account.
%% @spec list(Client::pid(), Container::string()) -> [string()]
list(ClientRef, Container) ->
	gen_server:call(herp_refreg:lookup(ClientRef), {list, Container}).

%% @doc
%% create_directory/2 will create a new directory in the Object Store
%% you can create any level directory you need.
%% @spec create_directory(ClientRef::pid(), Container::string()) -> ok | {error, Reason}
create_directory(ClientRef, Container) ->
	create_directory(ClientRef, Container, []).

%% @doc
%% create_directory/3 will create a new directory in the Object Store
%% along with all the Optional Metadata you require on your object.
%% @spec create_directory(ClientRef::pid(), Container::string(), Options::[{string(), string()}]) -> ok | {error, Reason}
create_directory(ClientRef, Container, Options) ->
    gen_server:call(herp_refreg:lookup(ClientRef), {create_directory, Container, Options}).

%% @doc
%% upload_file/2 will upload a file into the Object Store into the
%% container you specify.
upload_file(ClientRef, File, Container) ->
    upload_file(ClientRef, File, Container, []).

%% @doc
%% upload_file/3 will upload a file into the Object Store into the
%% container you specify along with any additional metadata you wish
%% to include.
upload_file(ClientRef, File, Container, Options) ->
    upload_file(ClientRef, File, Container, Options, 30000).

%% @doc
%% upload_file/3 will upload a file into the Object Store into the
%% container you specify along with any additional metadata you wish
%% to include as well as being able to specify the timeout.
upload_file(ClientRef, File, Container, Options, Timeout) ->
    case file:read_file(File) of
        {ok, FileContents} ->
            Filename = filename:basename(File),
            Headers = [
                       {"Content-Length", byte_size(FileContents)},
                       {"Content-Type", binary_to_list(hd(mimetypes:filename(File)))},
                       {"ETag", hash_file(FileContents)}
                      ] ++ Options,
            gen_server:call(herp_refreg:lookup(ClientRef),
                            {create_file, Container, FileContents, Filename, Headers}, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

copy_file(ClientRef, From, To) ->
    copy_file(ClientRef, From, To, []).

copy_file(ClientRef, From, To, Options) ->
    gen_server:call(herp_refreg:lookup(ClientRef),
                    {copy_file, From, To, Options}, 30000).

%% @doc
%% hash_file/1 will take a Binary and return the md5sum of that
%% binary.
%% @spec hash_file(Contents::binary()) -> string()
hash_file(Contents) ->
    << M: 128>> = crypto:md5(Contents),
    lists:flatten(io_lib:format("~32.15.0b", [M])).
