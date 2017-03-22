%%====================================================================
%%
%% This module supervisors all the particle modules
%%
%%====================================================================
-module(particle_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

%% API
-export([start_link/0, spawn_particle/2]).

%% Supervisor callbacks
-export([init/1]).


%% ------------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

spawn_particle(ParticleName, Options) ->
	% Spawn new thalamus (high level)
	{ok, NewParticlePiD} = supervisor:start_child(particle_sup, [ParticleName]),
	% io:format("NEW Particle NAME ~p~n", [ParticleName]),
	ebus:sub(NewParticlePiD, ParticleName), %% This needs to be here as we need the New-particle PID to subscribe it to a channel
	NewParticlePiD.


%% ------------------------------------------------------------------
%% Supervisor functions
%% ------------------------------------------------------------------
init([]) ->
	% If > MaxR number of restarts occur in the last MaxT seconds
	% the supervisor terminates all the child processes and then itself.
	SupFlags = #{strategy => simple_one_for_one,
				intensity => 0,    % Max restarts
				   period => 1},   % Max timeout


	Particle_Spec = #{id => particle,
				  start => {particle, start_link, []},
	 	  	   	restart => temporary,
	 		   shutdown => 5000,
		 	 	   type => worker,
	  	  	   	modules => [particle]},


	ChildSpecs = [Particle_Spec],
	ok = supervisor:check_childspecs(ChildSpecs),

	{ok, {SupFlags, ChildSpecs}}.


%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------