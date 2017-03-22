%%====================================================================
%%
%% @author Luke Taylor
%%
%% @doc The thalamus (from Greek θάλαμος, "chamber")[1] is a midline
%% symmetrical structure of two halves, within the vertebrate brain,
%% situated between the cerebral cortex and the midbrain. Some of its
%% functions are the relaying of sensory and motor signals to the cerebral
%% cortex,[2][3] and the regulation of consciousness, sleep, and alertness.
%%
%% This module is ultimately responsible for all I/O to and from the
%% artilect. It listens to all decision making modules, including cases
%% where multiple populations of the same neural net have been spawned,
%% and chooses what information goes to the outside world.
%%
%%====================================================================
-module(thalamus).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% API
-export([start_link/1, stop/0]).
-export([input/2,		%% Handle input to the Thalamus
		 output/2		%% Request for the Thalamus to perform output
		]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {artilect}).


%%====================================================================
%% API functions
%%====================================================================
start_link(Params) ->
	%% io:format("Thalamus name: ~p~n",[Params]),
	%% Explicitely not assigning an ID to prevent 'already registered errors'
	%% as multiple orbitos will spawn this module.
	gen_server:start_link(?MODULE, [Params], []).

stop() ->
	%% Stop server asynchronously
	io:format("Stopping~n"),
	gen_server:cast(?MODULE, shutdown).


%%% Handle input to the Thalamus
input(ArtilectName, Input) ->
	ebus:pub(ArtilectName, {thalamus, Input}).


%%% Request for the Thalamus to perform output
output(ArtilectName, Output) ->
	%% In this case simply print to screen
	ebus:pub(ArtilectName, {thalamus, print, Output}).


%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Params]) ->
	%% io:format("THALAMUS Initializing with ~p~n", [Params]),
	%% process_flag(trap_exit, true),
	NewState = #state{artilect=Params},
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


handle_info({thalamus, Inputs}, State) ->
	% Thalamus has received input, direct it to all sectors of the processing areas (lobes) of the artilect
	% io:format("ARTILECT ~p MODULE thalamus INPUTS: ~p~n", [State#state.name, Inputs]),
	ebus:pub(State#state.artilect, {lobe, Inputs}),
	{noreply, State};
handle_info({thalamus, print, Output}, State) ->
	% Thalamus has received input, direct it to all sectors of the processing areas (lobes) of the artilect
	% io:format("ARTILECT ~p MODULE thalamus INPUTS: ~p~n", [State#state.name, Inputs]),
	io:format("Artilect ~p - Thalamus print: ~p~n", [State#state.artilect, Output]),
	{noreply, State};
handle_info(_Message, _State) ->
	% io:format("Generic info handler: '~p' '~p'~n", [_Message, _State]),
	{noreply, _State}.
	%% Informative calls
	% {noreply,NewState}
	% {noreply,NewState,Timeout}
	% {noreply,NewState,hibernate}
	% {stop,Reason,NewState}

%% Server termination
terminate(_Reason, _State) ->
	io:format("Generic termination handler: '~p' '~p'~n", [_Reason, _State]).

%% Code change
code_change(_OldVersion, State, _Extra) ->
	{ok, State}.