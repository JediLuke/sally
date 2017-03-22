%%====================================================================
%%
%% This module supervisors all the lookup_table modules
%%
%%====================================================================
-module('lookup_table_sup_L3').
-behaviour(supervisor).
-define(SERVER, ?MODULE).

%% API
-export([start_link/0, spawn_lookup_table/2]).

%% Supervisor callbacks
-export([init/1]).


%% ------------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

spawn_lookup_table(Name, _Options) ->
	% Spawn new lookup_table (high level)
	{ok, New_lookup_table} = supervisor:start_child(lookup_table_sup_L3, [Name]),
	ebus:sub(New_lookup_table, Name),
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


	LookupTableSpec = #{id => lookup_table,
				 	start => {lookup_table, start_link, []},
	 	  	   	  restart => temporary,
	 		  	 shutdown => 5000,
		 	 	  	 type => worker,
	  	  	   	  modules => [lookup_table]},


	ChildSpecs = [LookupTableSpec],
	ok = supervisor:check_childspecs(ChildSpecs),

	{ok, {SupFlags, ChildSpecs}}.


%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------