%%====================================================================
%%
%% This module supervisors all the AI highest-level gen-servers
%%
%%====================================================================
-module('lobes_sup_L2').
-behaviour(supervisor).
-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ------------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ------------------------------------------------------------------
%% Supervisor functions
%% ------------------------------------------------------------------
init([]) ->
	% If > MaxR number of restarts occur in the last MaxT seconds
	% the supervisor terminates all the child processes and then itself.
	SupFlags = #{strategy => one_for_one,
				intensity => 3,    % Max restarts
				   period => 5},   % Max timeout


	%% Lobes (things that do calculation)
	%%----------------------------------------------------------------
	% PSO - Particle swarm of neural nets
	PSO_Sup = #{id => pso_sup,
			 start => {pso_sup, start_link, []},
		   restart => permanent,
		  shutdown => 5000,
			  type => supervisor,
		   modules => [pso_sup]},

	% Procedure engine - calculating part of the brain
	Procedure_Sup = #{id => procedure_sup,
				   start => {procedure_sup, start_link, []},
				 restart => permanent,
				shutdown => 5000,
					type => supervisor,
				 modules => [procedure_sup]},

	% Backprop_Sup = #{id => backprop_sup,
	% 			  start => {backprop_sup, start_link, []},
	% 			restart => permanent,
	% 		   shutdown => 5000,
	% 			   type => supervisor,
	% 			modules => [backprop_sup]},

	% Lookup table
	Lookup_Table_Sup = #{id => lookup_table_sup_L3,
					  start => {lookup_table_sup_L3, start_link, []},
					restart => permanent,
				   shutdown => 5000,
					   type => supervisor,
					modules => [lookup_table_sup_L3]},

	%% Future modules

	% Spatial reasoning model - "imagines" 3D spatial models
	% Concept calculus - Semantic based concept engine / Thoughtspace canvas
	% Graph search
	% Cellular automata engine / game of life
	% Analogy engine
	% Emotive center / reptilian stem / sexual drive
	% Random number generator - Because YOLO
	% Logical engine (subset of concept calculus?)
	% Lookup table - "The particle is a memory - it isn't a computer at all"
	% Signal filters
	% Pattern matcher
	% Memetic layer
	% Prediction engine
	% Autoassosciative memory
	% Syncronization module - Syncronizes lobes and ensures consistency and agreement between them.
	% 						The semantic module and the pattern matcher should both agree on an outcome


	%% NN Components
	% Exoself_Sup = #{id => exoself_sup,
	% 			 start => {exoself_sup, start_link, []},
	% 		   restart => permanent,
	% 		  shutdown => 5000,
	% 			  type => supervisor,
	% 		   modules => [exoself_sup]},

	Particle_Sup = #{id => particle_sup,
				start => {particle_sup, start_link, []},
			  restart => permanent,
			 shutdown => 5000,
				 type => supervisor,
			  modules => [particle_sup]},

	Scape_Sup = #{id => scape_sup,
				start => {scape_sup, start_link, []},
			  restart => permanent,
			 shutdown => 5000,
				 type => supervisor,
			  modules => [scape_sup]},

	Neuron_Sup = #{id => neuron_sup,
				start => {neuron_sup, start_link, []},
			  restart => permanent,
			 shutdown => 5000,
				 type => supervisor,
			  modules => [neuron_sup]},

	Cortex_Sup = #{id => cortex_sup,
				start => {cortex_sup, start_link, []},
			  restart => permanent,
			 shutdown => 5000,
				 type => supervisor,
			  modules => [cortex_sup]},



	ChildSpecs = [PSO_Sup, Procedure_Sup, Lookup_Table_Sup,
				  Particle_Sup, Scape_Sup, Neuron_Sup, Cortex_Sup],
	ok = supervisor:check_childspecs(ChildSpecs),

	{ok, {SupFlags, ChildSpecs}}.


%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------