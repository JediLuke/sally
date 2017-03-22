%%====================================================================
%%
%% This module supervisors all the cortex modules
%%
%%====================================================================
-module(cortex_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

%% API
-export([start_link/0, spawn_cortex/2]).

%% Supervisor callbacks
-export([init/1]).


%% ------------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

spawn_cortex(CortexName, NN_Config) ->
	{NetConfig, BiasVector} = NN_Config,
	% Spawn new thalamus (high level)
	% NN_Config = {{0.5, 0.5}, {0.5, 0.5, 0.5, 0.5}, {0.5, 0.5}},
	{ok, NewCortexPiD} = supervisor:start_child(cortex_sup, [CortexName, NetConfig, BiasVector]),
	% io:format("NEW CORTEX NAME ~p~n", [CortexName]),
	ebus:sub(NewCortexPiD, CortexName), %% This needs to be here as we need the New-Cortex PID to subscribe it to a channel
	NewCortexPiD.
% spawn_cortex(CortexName, NN_Config) ->
% 	% Spawn new thalamus (high level)
% 	{ok, NewCortexPiD} = supervisor:start_child(cortex_sup, [CortexName, NN_Config]),
% 	% io:format("NEW CORTEX NAME ~p~n", [CortexName]),
% 	ebus:sub(NewCortexPiD, CortexName), %% This needs to be here as we need the New-Cortex PID to subscribe it to a channel
% 	NewCortexPiD.



%% ------------------------------------------------------------------
%% Supervisor functions
%% ------------------------------------------------------------------
init([]) ->
	% If > MaxR number of restarts occur in the last MaxT seconds
	% the supervisor terminates all the child processes and then itself.
	SupFlags = #{strategy => simple_one_for_one,
				intensity => 0,    % Max restarts
				   period => 1},   % Max timeout


	Cortex_Spec = #{id => cortex,
				  start => {cortex, start_link, []},
	 	  	   	restart => temporary,
	 		   shutdown => 5000,
		 	 	   type => worker,
	  	  	   	modules => [cortex]},


	ChildSpecs = [Cortex_Spec],
	ok = supervisor:check_childspecs(ChildSpecs),

	{ok, {SupFlags, ChildSpecs}}.


%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------