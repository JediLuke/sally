%%====================================================================
%%
%% @author Luke Taylor
%%
%% @doc The orbitofrontal cortex (OFC) is a prefrontal cortex region in the frontal
%%      lobes in the brain which is involved in the cognitive processing of decision-making.
%%
%%====================================================================




% NOTES
% Really, the Orbito needs to keep track of the lobes in a lobe table, then for each input
% signal, it waits for the output. Also these lobes should report back a confidence level




-module(orbito).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% API
-export([start_link/1, stop/0]).
-export([lobe_update/3		%% Handle updates from Lobes
		]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% State variable
-record(state, {name, fitnessFunc, fitness}).


%%====================================================================
%% API functions
%%====================================================================
start_link(Params) ->
	%% io:format("Orbito name: ~p~n",[Params]),
	%% Explicitely not assigning an ID to prevent 'already registered errors'
	%% as multiple orbitos will spawn this module.
	gen_server:start_link(?MODULE, [Params], []).

stop() ->
	%% Stop server asynchronously
	io:format("Stopping~n"),
	gen_server:cast(?MODULE, shutdown).


%%% Handle updates from Lobes
lobe_update(ArtilectName, Lobe, LobeUpdate) ->
	io:format("ANAME: ~p LOBE ~p LOBEUP ~p~n", [ArtilectName, Lobe, LobeUpdate]),
	ebus:pub(ArtilectName, {lobe_update, Lobe, LobeUpdate}).


%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Params]) ->
	% io:format("ORBITO: Initializing with ~p~n", [Params]),
	%% process_flag(trap_exit, true),
	{ArtilectName, FitnessFunc} = Params,
	NewState = #state{name = ArtilectName, fitnessFunc = FitnessFunc, fitness = 0},
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
% handle_info({orbito, Inputs}, State) ->
% 	io:format("ORBITO INPUT: ~p~n", [Inputs]),
% 	{noreply, State};
handle_info({lobe_update, Lobe, LobeOutput}, State) ->
	NewState = process_lobe_output(State, LobeOutput),
	% io:format("ARTILECT ~p MODULE orbito UPDATE FROM ~p WITH RESULTING ACTIONS ~p~n", [State#state.name, Lobe, NewState]),
	{noreply, NewState};
handle_info(_Message, State) ->
	{noreply, State}.

%% Server termination
terminate(_Reason, _State) ->
	io:format("Generic termination handler: '~p' '~p'~n", [_Reason, _State]).

%% Code change
code_change(_OldVersion, State, _Extra) ->
	{ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================


process_lobe_output(State, LobeOutput) ->
	% This function decides if what the lobe just chucked back is worth anything...
	% Uses the Fitness function to decide if what we're doing is any good
	{_MinOrMax, FF} = State#state.fitnessFunc,
	CalcFitness = FF(LobeOutput),
	% Test current fitness against 
	io:format("CALC FITNESS: ~p~n", [CalcFitness]),
	% TODO Add test for min or max, but for now assume max
	if (CalcFitness > State#state.fitness) ->
		% Adjust State variable
		NewState = State#state{fitness = LobeOutput}, % Note, LobeOutput is the true output from the Lobe, Calcfitness is simply a heuristic
		print(NewState); %% In this case
	true ->
		NewState = State
	end,
	NewState.

print(State) ->
	% Communicate with outside world i.e. end API command
	thalamus:output(State#state.name, State#state.fitness),
	done.