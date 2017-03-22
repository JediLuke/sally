%%====================================================================
%%
%% This module supervisors all the AI highest-level gen-servers
%%
%%====================================================================
-module(pso_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).
-export([spawn_PSO/2]).

%% Supervisor callbacks
-export([init/1]).


%% ------------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

spawn_PSO(ArtilectName, Options) ->
	% Spawn new PSO (high level)
	{ok, New_PSO} = supervisor:start_child(pso_sup, [ArtilectName]),
	pso:new_swarm(ArtilectName, Options), % This must be after subscribing to the channel, as new_swarm uses that functionality
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


	PSO_Spec = #{id => pso,
			  start => {pso, start_link, []},
			restart => temporary,
		   shutdown => 5000,
			   type => worker,
			modules => [pso]},


	ChildSpecs = [PSO_Spec],
	ok = supervisor:check_childspecs(ChildSpecs),

	{ok, {SupFlags, ChildSpecs}}.


%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------