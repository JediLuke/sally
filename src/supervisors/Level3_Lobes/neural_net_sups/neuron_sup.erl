%%====================================================================
%%
%% This module supervisors all the neuron modules
%%
%%====================================================================
-module(neuron_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

%% API
-export([start_link/0, spawn_neuron/2]).

%% Supervisor callbacks
-export([init/1]).


%% ------------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

spawn_neuron(NN_Name, NeuronGene) ->
	% {NeuronId, Weighting, Bias, ConnectionList} = NeuronGene,
	% Spawn new thalamus (high level)
	% {ok, New_neuron} = supervisor:start_child(neuron_sup, [NetName, NeuronId, Weighting, Bias, ConnectionList]),
	{ok, NewNeuronPiD} = supervisor:start_child(neuron_sup, [NN_Name, NeuronGene]),
	% NetName = list_to_atom(return_particle_name(atom_to_list(Name))),
	ebus:sub(NewNeuronPiD, NN_Name),
	NewNeuronPiD.


%% ------------------------------------------------------------------
%% Supervisor functions
%% ------------------------------------------------------------------
init([]) ->
	% If > MaxR number of restarts occur in the last MaxT seconds
	% the supervisor terminates all the child processes and then itself.
	SupFlags = #{strategy => simple_one_for_one,
				intensity => 0,    % Max restarts
				   period => 1},   % Max timeout


	Neuron_Spec = #{id => neuron,
				  start => {neuron, start_link, []},
	 	  	   	restart => temporary,
	 		   shutdown => 5000,
		 	 	   type => worker,
	  	  	   	modules => [neuron]},


	ChildSpecs = [Neuron_Spec],
	ok = supervisor:check_childspecs(ChildSpecs),

	{ok, {SupFlags, ChildSpecs}}.


%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------