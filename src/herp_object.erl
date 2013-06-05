-module(herp_object).

%% Public API
-export([list/1, list/2, create_directory/2, create_directory/3, upload_file/3, upload_file/4, upload_file/5]).

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
%% @spec create_directory(ClientRef::pid(), Container::string(), Options::[{string(), string()}}) -> ok | {error, Reason}
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
            Headers = [{"Content-Length", byte_size(FileContents)},
                       {"Content-Type", binary_to_list(hd(mimetypes:filename(File)))}] ++ Options,
            gen_server:call(herp_refreg:lookup(ClientRef),
                            {create_file, Container, FileContents, Filename, Headers}, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.
