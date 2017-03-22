%%%-------------------------------------------------------------------
%%		Sally :)
%%%-------------------------------------------------------------------
-module(sally_sup_top).
-behaviour(supervisor).
-define(SERVER, ?MODULE).


%% API
-export([start_link/0]).	%% Starts the supervisor


%% Supervisor callbacks
-export([init/1]).


%%====================================================================
%% API functions
%%====================================================================
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
	% If > MaxR number of restarts occur in the last MaxT seconds
	% the supervisor terminates all the child processes and then itself.
	SupFlags = #{strategy => one_for_one,
				intensity => 1,    % Max restarts
				   period => 5},   % Max timeout


	%% External Interfaces
	%%----------------------------------------------------------------

	%% Webserver
	% Yaws = #{id => ybed_sup,
	% 	  start => {ybed_sup, start_link, []},
	% 	restart => permanent,
	%    shutdown => 5000,
	% 	   type => supervisor,
	% 	modules => [ybed_sup]},


	%% Internal modules
	%%----------------------------------------------------------------

	% Artilect supervisors
	% Artilect is supposed to be the highest level interface to the artilect,
	% But it's looking like it might be completely redundant

	% This module supervisors all the AI highest-level gen-servers
	% Artilect_Sup = #{id => artilect_sup_L2,
	% 			  start => {artilect_sup_L2, start_link, []},
	% 	  		restart => permanent,
	% 	 	   shutdown => 5000,
	% 		 	   type => supervisor,
	% 	  		modules => [artilect_sup_L2]},

	% This is a test supervisor that isn't simple_one_for_one
	% AI_Sup2 = #{id => ai_sup_t,
	% 		 start => {ai_sup_t, start_link, []},
	% 	   restart => permanent,
	% 	  shutdown => 5000,
	% 		  type => supervisor,
	% 	   modules => [ai_sup_t]},

	% Orbito - Decision making module in brain
	Orbito_Sup = #{id => orbito_sup_L2,
				start => {orbito_sup_L2, start_link, []},
			  restart => permanent,
			 shutdown => 5000,
				 type => supervisor,
			  modules => [orbito_sup_L2]},

	% Thalamus - Handles all I/O in the artilect
	Thalamus_Sup = #{id => thalamus_sup,
				  start => {thalamus_sup, start_link, []},
				restart => permanent,
			   shutdown => 5000,
				   type => supervisor,
				modules => [thalamus_sup]},

	% Lobe supervisor - lobes are where computation takes place
	Lobes_Sup = #{id => lobes_sup_L2,
			   start => {lobes_sup_L2, start_link, []},
			 restart => permanent,
			shutdown => 5000,
				type => supervisor,
			 modules => [lobes_sup_L2]},


	ChildSpecs = [Orbito_Sup, Thalamus_Sup, Lobes_Sup],
	ok = supervisor:check_childspecs(ChildSpecs),

	{ok, {SupFlags, ChildSpecs}}.


%%====================================================================
%% Internal functions
%%====================================================================