%%====================================================================
%%
%% This module supervisors all the Orbito modules
%%
%%====================================================================
-module('orbito_sup_L2').
-behaviour(supervisor).
-define(SERVER, ?MODULE).

%% API
-export([start_link/0, spawn_orbito/3]).

%% Supervisor callbacks
-export([init/1]).


%% ------------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

spawn_orbito(Name, FitnessFunc, _Options) ->
	% Spawn new orbito (high level)
	{ok, New_Orbito} = supervisor:start_child(orbito_sup_L2, [{Name, FitnessFunc}]),
	ebus:sub(New_Orbito, Name),
	ok.


%% ------------------------------------------------------------------
%% Supervisor functions
%% ------------------------------------------------------------------
init([]) ->
	% If > MaxR number of restarts occur in the last MaxT seconds
	% the supervisor terminates all the child processes and then itself.
	SupFlags = #{strategy => simple_one_for_one,
				intensity => 0,    % Max restarts
				   period => 1},   % Max timeout


	Orbito_Spec = #{id => orbito,
				 start => {orbito, start_link, []},
	 	  	   restart => temporary,
	 		  shutdown => 5000,
		 	 	  type => worker,
	  	  	   modules => [orbito]},


	ChildSpecs = [Orbito_Spec],
	ok = supervisor:check_childspecs(ChildSpecs),

	{ok, {SupFlags, ChildSpecs}}.


%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------