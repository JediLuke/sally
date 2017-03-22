%%====================================================================
%%
%% This module supervisors all the thalamus modules
%%
%%====================================================================
-module(thalamus_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

%% API
-export([start_link/0, spawn_thalamus/2]).

%% Supervisor callbacks
-export([init/1]).


%% ------------------------------------------------------------------
%% API functions
%% ------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

spawn_thalamus(Name, _Options) ->
	% Spawn new thalamus (high level)
	{ok, New_thalamus} = supervisor:start_child(thalamus_sup, [Name]), % Looks like this needs to go in a tuple for some reason...
	ebus:sub(New_thalamus, Name),
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


	Thalamus_Spec = #{id => thalamus,
				   start => {thalamus, start_link, []},
	 	  	   	 restart => temporary,
	 		  	shutdown => 5000,
		 	 		type => worker,
	  	  	   	 modules => [thalamus]},


	ChildSpecs = [Thalamus_Spec],
	ok = supervisor:check_childspecs(ChildSpecs),

	{ok, {SupFlags, ChildSpecs}}.


%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------