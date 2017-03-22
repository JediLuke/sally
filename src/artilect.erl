-module(artilect).
%%%-------------------------------------------------------------------
%%% Artilect module:
%%% Groups together all interactions with artilects
%%%-------------------------------------------------------------------

-export([new/0, new/1, new/2,	%% Spawn a new artilect
		 input/2,				%% Give an artilect input
		 train/1				%% Starts an artilect computing stuff
		 ]).


%%====================================================================
%% API
%%====================================================================


%%% Spawn a new artilect using defaults
new() ->
	ArtilectName = new_name(), %% TODO See function
	%% Default values
	Options = {pso, 5}, % {pso, NumParticles}
	FitnessFunc = {max, fun(X) -> %% TODO need description of FitnessFunc
		Return = (X*X - 2),
		Return
		end},
	%% Spawn artilect
	ok = spawn_artilect(ArtilectName, FitnessFunc, Options),
	ArtilectName.
%%% Spawn a new artilect with defined fitness func
new(FitnessFunc) ->
	ArtilectName = new_name(),
	%% Default values
	Options = {pso, 2}, % {pso, NumParticles}
	%% Spawn artilect
	ok = spawn_artilect(ArtilectName, FitnessFunc, Options),
	ArtilectName.
%%% Spawn new artilect with defined fitness func and options
new(FitnessFunc, Options) ->
	ArtilectName = new_name(),
	%% Spawn artilect
	ok = spawn_artilect(ArtilectName, FitnessFunc, Options),
	ArtilectName.


%%% Send an artilect some input
input(ArtilectName, Input) ->
	thalamus:input(ArtilectName, Input).	% I/O is controlled by the Thalamus


%%% Start training an artilect
train(ArtilectName) ->
	%% TODO eventually this will be more than just PSO
	%% This should send all lobes a signal to begin
	pso:train(ArtilectName).


%%% Stop an artilect training
% stop_training(ArtilectName) ->
% 	error. %% TODO implement


%%====================================================================
%% Internal functions
%%====================================================================


% Create a new artilect
-spec spawn_artilect(atom(), tuple(), tuple()) -> ok.
spawn_artilect(ArtilectName, FitnessFunc, Options) ->
	% Spawn Cortex modules
	% -------------------------------
	% artilect_sup_L2:spawn_artilect(Name, Options), %% For now, artilects actually seem redundant...
	orbito_sup_L2:spawn_orbito(ArtilectName, FitnessFunc, Options),
	thalamus_sup:spawn_thalamus(ArtilectName, Options),
	% io:format("OPT: ~p~n", [Options]),

	% Spawn Lobe modules
	% -------------------------------
	case Options of % TODO This isn't right, we will eventually have lots of lobes, but right now the sync problem means let's just run one at a time
		{pso, _NumParticles} ->
			pso_sup:spawn_PSO(ArtilectName, Options);
		{procedure} ->
			procedure_sup:spawn_procedure(ArtilectName, Options);
		{lookup_table} ->
			lookup_table_sup_L3:spawn_lookup_table(ArtilectName, Options);
		_Else ->
			false
	end.


-spec new_name() -> atom.
new_name() ->
	karly. %% TODO Make not hard coded :( random ?