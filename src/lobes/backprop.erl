%%====================================================================
%%
%% @author Luke Taylor
%%
%% @doc The backprop is responsible for deciding what lobes are correct in their reasoning
%%
%%====================================================================
-module(backprop).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% API
-export([start_link/1, stop/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% State variable
-record(state, {artilect, lobeName}).


%%====================================================================
%% API functions
%%====================================================================
start_link(Params) ->
    %% io:format("backprop name: ~p~n",[Params]),
    %% Explicitely not assigning an ID to prevent 'already registered errors'
    %% as multiple backprops will spawn this module.
    gen_server:start_link(?MODULE, [Params], []).

stop() ->
	%% Stop server asynchronously
    io:format("Stopping~n"),
    gen_server:cast(?MODULE, shutdown).


%%====================================================================
%% gen_server callbacks
%%====================================================================
init([_Params]) ->
    %% io:format("backprop: Initializing with ~p~n", [Params]),
    %% process_flag(trap_exit, true),
     NewState = #state{artilect=_Params, lobeName="backprop"},
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
handle_info({lobe, Inputs}, State) ->
    % Handle all information sent out to lobes (processing modules)
    % io:format("ARTILECT ~p LOBE procedure INPUTS: ~p~n", [State#state.name, Inputs]),
    % Think about these inputs
    Outputs = process_input(Inputs),
    % Send these back to the orbito, who shall judge their worthiness
    ebus:pub(State, {lobe_update, State#state.lobeName, Outputs}),
    % io:format("ARTILECT ~p LOBE procedure - done thinking :)~n", [State#state.name]),
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


%%====================================================================
%% Internal functions
%%====================================================================

process_input(Inputs) ->
    io:format("Backprop Inputs: ~p~n", [Inputs]),
    ok.