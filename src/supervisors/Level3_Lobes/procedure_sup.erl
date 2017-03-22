%%====================================================================
%%
%% This module supervisors all the procedure modules
%%
%%====================================================================
-module('procedure_sup').
-behaviour(supervisor).
-define(SERVER, ?MODULE).

%% API
-export([start_link/0, spawn_procedure/2]).

%% Supervisor callbacks
-export([init/1]).


%% ------------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

spawn_procedure(Name, _Options) ->
	% Spawn new procedure (high level)
	{ok, New_Procedure} = supervisor:start_child(procedure_sup, [Name]),
	ebus:sub(New_Procedure, Name),
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


	Procedure_Spec = #{id => procedure,
				 	start => {procedure, start_link, []},
	 	  	   	  restart => temporary,
	 		  	 shutdown => 5000,
		 	 	  	 type => worker,
	  	  	   	  modules => [procedure]},


	ChildSpecs = [Procedure_Spec],
	ok = supervisor:check_childspecs(ChildSpecs),

	{ok, {SupFlags, ChildSpecs}}.


%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------