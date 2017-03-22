%%====================================================================
%%
%% @author Luke Taylor
%% 
%% This module is a new behaviour things will implement, which shall
%% handle things like spawning children, the Gen_server BS, memory and pub/sub
%% All through a nice API
%%
%% Furthermore, this module should inherintley allow 'clustering' -> Nodes
%% can parallelize their work by using spawned processes to handle requests
%%
%%====================================================================
-module(node).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% API
-export([start_link/1, stop/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {name}).


-callback test(Args :: term()) ->
      {ok,   State  :: term()}
    | ignore.

%%====================================================================
%% API functions
%%====================================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Shortcut
start_link(Params) ->
    %% io:format("node name: ~p~n",[Params]),
    %% Explicitely not assigning an ID to prevent 'already registered errors'
    %% as multiple orbitos will spawn this module.
    gen_server:start_link(?MODULE, [Params], []).

stop() ->
    %% Stop server asynchronously
    io:format("Stopping~n"),
    gen_server:cast(?MODULE, shutdown).


%%====================================================================
%% gen_server callbacks
%%====================================================================
init([_Params]) ->
    %% io:format("node Initializing with ~p~n", [Params]),
    %% process_flag(trap_exit, true),
    NewState = #state{name=_Params},
    {ok, NewState}.

%% Synchronous, possible return values
% {reply,Reply,NewState}
% {reply,Reply,NewState,Timeout}
% {reply,Reply,NewState,hibernate}
% {noreply,NewState}
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,Reply,NewState}
% {stop,Reason,NewState}
handle_call(Message, From, State) ->
    io:format("Generic call handler: '~p' from '~p' while in '~p'~n", [Message, From, State]),
    {reply, ok, State}.

%% Asynchronous, possible return values
% {noreply,NewState}
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
%% normal termination clause
handle_cast(shutdown, State) ->
    io:format("Generic cast handler: *shutdown* while in '~p'~n", [State]),
    {stop, by_request, State};
%% generic async handler
handle_cast(Message, State) ->
    io:format("Generic cast handler: '~p' while in '~p'~n", [Message, State]),
    {noreply, State}.

%% Informative calls
% {noreply,NewState}
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
% {stop,Reason,NewState}
handle_info({nn, soundoff}, State) ->
    io:format("node MODULE: Name: ~p~n", [State#state.name]),
    {noreply, State};
handle_info({node, Inputs}, State) ->
    io:format("node INPUT: ~p~n", [Inputs]),
    {noreply, State};
handle_info(_Message, _State) ->
    % io:format("Generic info handler: '~p' '~p'~n", [_Message, _State]),
    {noreply, _State}.

%% Server termination
terminate(_Reason, _State) ->
    io:format("Generic termination handler: '~p' '~p'~n", [_Reason, _State]).

%% Code change
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.