%%====================================================================
%%
%% This module supervisors all the AI highest-level gen-servers
%%
%%====================================================================
-module('backprop_sup').
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
	SupFlags = #{strategy => simple_one_for_one,
				intensity => 0,    % Max restarts
				   period => 1},   % Max timeout


	Backprop_Spec = #{id => backprop,
				   start => {backprop, start_link, []},
				 restart => temporary,
				shutdown => 5000,
					type => worker,
				 modules => [backprop]},


	ChildSpecs = [Backprop_Spec],
	ok = supervisor:check_childspecs(ChildSpecs),

	{ok, {SupFlags, ChildSpecs}}.


%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------