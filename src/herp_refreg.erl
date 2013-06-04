%%%-------------------------------------------------------------------
%%% File    : herp_refreg.erl
%%% Author  :  <xeno@localhost>
%%% Description : 
%%%
%%% Created :  4 Jun 2013 by  <xeno@localhost>
%%%-------------------------------------------------------------------
-module(herp_refreg).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0, register/2, lookup/1, delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {refs}).

-define(SERVER, ?MODULE).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc
%% register/2 registers a new Reference with a Pid so we can have a
%% reference to a possibly always changing Pid.
%% @spec register(Ref::ref(), Pid::pid()) -> ok
register(Ref, Pid) ->
    gen_server:call(?SERVER, {register, {Ref, Pid}}).

%% @doc
%% lookup/1 looks up a reference for it's associate Pid.
%% @spec lookup(Ref::ref()) -> pid() | {error, Reason}
lookup(Ref) ->
    gen_server:call(?SERVER, {lookup, Ref}).

%% @doc
%% delete/1 deletes any mapping between the Ref::ref() and the Pid::pid()
%% @spec delete(Ref::ref()) -> ok
delete(Ref) ->
    gen_server:call(?SERVER, {delete, Ref}).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{refs = dict:new()}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({register, {Ref, Pid}}, _From, #state{refs = Refs}) ->
    Reply = ok,
    {reply, Reply, #state{refs = dict:store(Ref, Pid, Refs)}};

handle_call({lookup, Ref}, _From, #state{refs = Refs} = State) ->
    case dict:is_key(Ref, Refs) of
        true ->
            Reply = dict:fetch(Ref, Refs),
            {reply, Reply, State};
        _Else ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete, Ref}, _From, #state{refs = Refs} = State) ->
    Reply = ok,
    case dict:is_key(Ref, Refs) of
        true ->
            {reply, Reply, #state{refs = dict:erase(Ref, Refs)}};
        _Else ->
            {reply, Reply, State}
    end.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
