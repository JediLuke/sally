%%====================================================================
%%
%% This module supervisors all the scape modules
%%
%%====================================================================
-module(scape_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

%% API
-export([start_link/0, spawn_scape/2]).

%% Supervisor callbacks
-export([init/1]).


%% ------------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

spawn_scape(Name, _Options) ->
	% Spawn new thalamus (high level)
	{ok, NewScapePiD} = supervisor:start_child(scape_sup, [Name]),
	ebus:sub(NewScapePiD, Name),
	NewScapePiD.


%% ------------------------------------------------------------------
%% Supervisor functions
%% ------------------------------------------------------------------
init([]) ->
	% If > MaxR number of restarts occur in the last MaxT seconds
	% the supervisor terminates all the child processes and then itself.
	SupFlags = #{strategy => simple_one_for_one,
				intensity => 0,    % Max restarts
				   period => 1},   % Max timeout


	Scape_Spec = #{id => scape,
				  start => {scape, start_link, []},
	 	  	   	restart => temporary,
	 		   shutdown => 5000,
		 	 	   type => worker,
	  	  	   	modules => [scape]},


	ChildSpecs = [Scape_Spec],
	ok = supervisor:check_childspecs(ChildSpecs),

	{ok, {SupFlags, ChildSpecs}}.


%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------