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

%% When a Pid is retrieved we check if it's an alive Pid, if not we
%% wait until this time has passed and then recheck it.
-define(PID_DEATH_TIMER, 3000).
%% If the Pid is truly dead and is not coming back, for example when
%% herp_client:quit/1 was called on it, then we must be able to
%% unblock callers after a given time. We define just how many times
%% to timeout before giving up.
-define(MAX_PID_DEATH_TIMEOUTS, 3).


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
    Pid = gen_server:call(?SERVER, {lookup, Ref}),
    case is_process_alive(Pid) of
        true ->
            Pid;
        _Else ->
            timer:sleep(?PID_DEATH_TIMER),
            lookup(Ref, ?MAX_PID_DEATH_TIMEOUTS)
    end.

%% @doc
%% lookup/2 is the fallback for when a valid Ref to Pid mapping cannot
%% be established. We loop whilst not is_process_alive waiting until
%% our max timeouts has occured, if it does, we assume that the
%% reference has become permanently invalid.
%% @spec lookup(_Ref::ref(), 0) -> {error, invalid_ref}
lookup(_Ref, 0) ->
    {error, invalid_ref};
%% @spec lookup(Ref::ref(), N) -> Pid::pid()
lookup(Ref, N) ->
    Pid = gen_server:call(?SERVER, {lookup, Ref}),
    case is_process_alive(Pid) of
        true ->
            Pid;
        _Else ->
            timer:sleep(?PID_DEATH_TIMER),
            lookup(Ref, N-1)
    end.

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
