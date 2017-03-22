-module(pso).
-behaviour(gen_server).
%%%=============================================================================
%%% Particle Swarm Optimization module
%%%
%%% @author Luke Taylor
%%% @doc This module, the pso controller, implements particle swarm optimization in parallel
%%% Rather than keep track of PiDs and doing messaing this way, ebus - a pub/sub application is used
%%%=============================================================================


% How this module works -

% PSO is a gen_server, we make a new PSO for each artilect.

% When a PSO is created ( pso_sup:spawn_PSO() ) a new PSO gen_server is spawned, and we immedaitely 
% call pso:new_swarm() on it. Thus a bunch of swarm particles (in this case particles) are created.

% Here is the call stack:
% artilect:train() -> pso:train() -> kick() -> particle:kickstart() (On each particle particle we made in new_swarm() )

% particle:kickstart() initializes particles with some values, then starts the particle 'cycle', where it runs
% against a simulation (scape), eventually returning it's reaults and reporting them back to PSO gen_server

% The PSO gen_server is tasked with keeping track of the progress for the swarms running in parallel, and
% as each checks in with their laest results, it updates those particles with the swarm wide knowledge of
% the global bestConfig, allowing that particle to begin it's next 'cycle' with better knowledge (i.e. change
% it's velocity).

% PSO
% 1 - Kickstart. This spawns all the particles in the swarm, but doesn't give them any settings yet
% 2 - Train. Calls 'kick', which in turn calls 'kickstart' on each particle
% PARTICLE
% 3 - Kickstart. Particles are given random weightings, velocities and bias'. Then it calls 'cycle' on each aprticle
% 4 - Cycle. Cycle applied the velocity to the current state (i.e. moves the particle) then runs tests to get new values for how well these values perform
% 5 - run_tests. Runs the test suits inside particle - in our case, it spawns some scapes and calls test_particle, and passes in the data to test
% SCAPE
% (spawn_scape just initializes the scape but no settings)
% 6 - test_particle calls xorr_neurtal, which is our test. We run it on the data passed in from the particle process
% 7 - To perform the tests in our case, we spawn a cortex and pass in the NN_Config, which is what we're really testing here. Then pass in test data with sensor_input
% CORTEX
% (cortex is initialized with spawn_cortex, called in previous step by scape)
% sensor_input. Passes input data to relevant sensor neurons
% NEURON (sensors)
% (neurons were created and initialized inside the spawn_cortex function called in step 7)
% filter. Neurons filter on their name to receive data messages. {s1, Data} is picked up by s1. When we get to an actuator, it calls cortex:actuator_output
% CORTEX (again)
% actuator_output. Sends results back to the scape and then ends itself, the Cortex process.
% SCAPE (again)
% return_results. Calculates final error etc. and sends it back up to particle, then ends itself (the scape process)
% PARTICLE (again)
% cycle_two. Tallies up results - if all 4 scapes have finished, kick it back up to PSO for final tally
% PSO (again)
% particle_checkin. Checks results. If gBest unset, set it with results and cycle particle again (this is the first result back.
% 	% If results are the new gBest, update and call adjustment
%	% If results are not the new gBest call adjustment

% This shall continue until particle_checkin either hits it's count limit, or until we find an acceptable value 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Header information
%%% API Exports
-export([start_link/1, stop/0]).		%% Required by gen_server
-export([train/1,						%% Manual kickstart of the PSO - combines a few steps into one
		 new_swarm/2,					%% Set up a new swarm
		 %set_swarm_size/1,				%% Adjust number of particles in swarm
		 input/2,						%% Information sent in to the PSO
		 particle_checkin/1			%% Particles use this method to check-in and update swarm status
		]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% State variable
-record(state, {artilect,				%% Name of the artilect
				lobeName,
				gBest,
				bestConfig,
				count,
				swarmSize
		}).

%%% Macros
-define(SERVER, ?MODULE).


%%% NOTE/TODO: Eventually, this module shall have it's own NN seperate from the PSO to answer axternal queries.
%%% The PSO is simply to discover the best genetic & memetic configuration


%%%=============================================================================
%%% API functions
%%%=============================================================================



% Eventually we will need:
% - initializeSwarm (creates particles and assigns random starting points)
% - solve (starts solving procedure)
% - stopSolve (stops solving, but doesn't remove particles from memory)
% - getters (best answer so far etc.)
% - save (saves state of search somehow)
% - end (stops solving and removes particles from memory)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API
%%% kickstart - Manually begin the PSO process for some pre-defined problem (testing)
train(ArtilectName) -> 
	ebus:pub(ArtilectName, {pso, kickstart, ArtilectName}),
	ok.

%%% Sets the number of particles in the swarm
% set_swarm_size(_Size) -> %% TODO Implement
% 	ok.

%%% Create a new swarm
new_swarm(ArtilectName, {pso, Size}) ->
	ebus:pub(ArtilectName, {new_swarm, {ArtilectName, Size}}),
	ok.

%%% Allows input from external parts of the program
input(ArtilectName, Inputs) ->
	ebus:pub(ArtilectName, {lobe, Inputs}).

%%% This is what particles use to talk back to the PSO coordinator
particle_checkin(Input) ->
	{karly, {{pso_update, ParticleName}, ParticleConfig, ParticleVelocity, ScapeResults}} = Input, %% TODO make this non-hacky
	% io:format("P_Check: Particle: ~p P_Config: ~p P_Vel: ~p Results: ~p~n", [ParticleName, ParticleConfig, ParticleVelocity, ScapeResults]),
	ebus:pub(karly, {{pso_update, ParticleName}, ParticleConfig, ParticleVelocity, ScapeResults}), %% TODO HACKY HACKY don't use hard coded broadcast channel in epub
	ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Required by gen_server
%%% start link
start_link(ArtilectName) ->
	%% io:format("pso name: ~p~n", [ArtilectName]),
	%% Explicitely not assigning an ID to prevent 'already registered errors'
	%% as multiple psos will spawn this module.
	{ok, New_PSO} = gen_server:start_link(?MODULE, [ArtilectName], []),
	%% Subscribe this PSO lobe to the Artilect's PubSub channel
	ebus:sub(New_PSO, ArtilectName),
	{ok, New_PSO}.

%%% stop
stop() ->
	%% Stop server asynchronously
	io:format("Stopping~n"),
	gen_server:cast(?MODULE, shutdown).


%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% init
init([ArtilectName]) ->
	%% io:format("pso: Initializing with ~p~n", [ArtilectName]),
	%% process_flag(trap_exit, true),
	NewState = #state{artilect 	 = ArtilectName,
					  lobeName 	 = pso,
					  gBest	   	 = unset,
					  bestConfig = unset,
					  count 	 = 0,
					  swarmSize  = 0},

	{ok, NewState}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% handle_call
handle_call(Message, From, State) ->
	io:format("Generic call handler: '~p' from '~p' while in '~p'~n", [Message, From, State]),
	{reply, ok, State}.
	%% Synchronous, possible return values
	%% {reply,Reply,NewState} | {reply,Reply,NewState,Timeout} | {reply,Reply,NewState,hibernate} | {noreply,NewState} |
	%% {noreply,NewState,Timeout} | {noreply,NewState,hibernate} |  {stop,Reason,Reply,NewState} | {stop,Reason,NewState}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% handle_cast
%%% normal termination clause
handle_cast(shutdown, State) ->
	io:format("Generic cast handler: *shutdown* while in '~p'~n", [State]),
	{stop, by_request, State};
%%% generic async handler
handle_cast(Message, State) ->
	io:format("Generic cast handler: '~p' while in '~p'~n", [Message, State]),
	{noreply, State}.
	%% Asynchronous, possible return values
	%% {noreply, NewState} | {noreply, NewState, Timeout} | {noreply, NewState, hibernate} | {stop, Reason, NewState}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% handle_info
%%% Handle an update from a particle in the swarm
handle_info({{pso_update, Particle}, Config, _Velocity, OutputError}, State) ->
	CountLimit = 5000, %% TODO waste of memory & hard-coded but who GAF
	NewCount = State#state.count + 1,
	% io:format("--- Newcount: ~p ---~n", [NewCount]), %% Just a visual so we can see when particles are swarming
	% io:format("--> ~p PSO Update - P: ~p Config: ~p Output: ~p ::: Count ~p~n" , [State#state.artilect, Particle, Config, Output, NewCount]),
	% io:format("~p -->~n", [NewCount]),
	% Print an updates
	% io:format("gBEST??? ~p OutputError ~p~n", [State#state.gBest, OutputError]),
	RemRem = (NewCount rem 50),
	if
		RemRem == 0 ->
			%io:format("--- Update:: count: ~p gBest-Error ~p bestConfig ~p~n", [NewCount, State#state.gBest, State#state.bestConfig]);
			io:format("--- PSO Update:: count: ~p gBest(min error) ~p Config: ~p~n", [NewCount, State#state.gBest, State#state.bestConfig]);
			%ok;
		true ->
			pass
	end,
	if
		%% First particle to report back -> set initial values
		State#state.gBest == 'unset' ->
			NewState = State#state{gBest = OutputError, bestConfig = Config, count=NewCount},
			particle:cycle(Particle);
		% Found an ok solution
		OutputError < 0.05 ->
			NewState = State#state{count=NewCount},
			io:format("~nGOOD RESULTS: count: ~p gBest-Error ~p bestConfig ~p~n", [NewCount, State#state.gBest, State#state.bestConfig]);
		%% Swarm has gone on too long -> print results
		NewCount == CountLimit ->
			NewState = State#state{count=NewCount},
			io:format("~nRESULTS: count: ~p gBest-Error ~p bestConfig ~p~n", [NewCount, State#state.gBest, State#state.bestConfig]);
		%% This catches the results being displayed as more particled finish after the limit TODO probably a clearner way possible here
		NewCount > CountLimit ->
			NewState = State;
		%% Particle has not exceed global best, therefore particle must update not controller - %% Minimize error
		(State#state.gBest /= 'unset') and (State#state.gBest =< OutputError) -> 
			NewState = State#state{count=NewCount},
			% io:format("AAA~n"), %% TODO SHOULD THIS JUST BE STATE????????????????????????? NOT NEWSTATE
			particle:adjustment(Particle, {adjustment, NewState#state.gBest, NewState#state.bestConfig});
		%% Particle has exceeded global best, therefore controller must update not particle, that may continue on current velocity
		(State#state.gBest /= 'unset') and (State#state.gBest > OutputError) ->
			% io:format("OLD gBEST: ~p new gBest: ~p~n", [State#state.gBest, OutputError]),
			NewState = State#state{gBest = OutputError, bestConfig=Config, count=NewCount},
			% io:format("BBB~n"),
			% particle:cycle(Particle)
			particle:adjustment(Particle, {adjustment, NewState#state.gBest, NewState#state.bestConfig})
	end,
	{noreply, NewState};
%%% Respond to external input i.e. messages from the Thalamus - TODO
handle_info({lobe, Inputs}, State) ->
	% Handle all information sent out to lobes (processing modules)
	% io:format("ARTILECT ~p LOBE procedure INPUTS: ~p~n", [State#state.artilect, Inputs]),
	% Think about these inputs
	Outputs = process_inputs(Inputs),
	% Send these back to the orbito, who shall judge their worthiness
	orbito:lobe_update(State#state.artilect, State#state.lobeName, Outputs),
	{noreply, State};
%%% Kickstart the PSO algorithm
handle_info({pso, kickstart, ArtilectName}, State) ->
	NumParticles = State#state.swarmSize,
	kick(ArtilectName, NumParticles),
	{noreply, State};
%%% Create a new swarm of particles
handle_info({new_swarm, {ArtilectName, Size}}, State) ->
	% io:format("New Swarm - NAME: ~p SIZE: ~p~n", [ArtilectName, Size]),
	create_swarm(ArtilectName, Size),
	%% Add info about number of particles to PSO coordinator state
	NewState = State#state{swarmSize = Size},
	{noreply, NewState};
%%% generic info handler
handle_info(_Message, State) ->
	% io:format("Generic info handler: Msg: '~p' State: '~p'~n", [Message, State]),
	{noreply, State}.
	%% Informative calls
	%% {noreply, NewState} | {noreply, NewState, Timeout} | {noreply, NewState, hibernate} |
	%% {stop, Reason, NewState} | {stop, Reason, NewState}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% server termination
terminate(_Reason, _State) ->
	io:format("Generic termination handler: '~p' '~p'~n", [_Reason, _State]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% code change
code_change(_OldVersion, State, _Extra) ->
	{ok, State}.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================


process_inputs(Inputs) ->
	%% TODO This should query the best particle we have and get it's input
	%% For now we shall just echo them back.
	Inputs.

%%% This function spawns a NumParticles amount of new particles using the new_partical function
%%% Calls itself recursively until NumParticles = 0
create_swarm(ArtilectName, 1) ->
	new_particle(ArtilectName, 1);
create_swarm(ArtilectName, NumParticles) when NumParticles >= 2, is_integer(NumParticles) ->
	new_particle(ArtilectName, NumParticles),
	create_swarm(ArtilectName, NumParticles-1). % Note: Must pass ArtilectName again, not ParticleName - DOH!


%%% This function uses ArtilectName and the number of particles to make a ParticleName, then
%%% assigns a particle name to each, then kicks them off
kick(ArtilectName, 1) ->
	ParticleName = list_to_atom(atom_to_list(ArtilectName) ++ integer_to_list(1)), %% TODO I bet this is slow...
	particle:kickstart(ParticleName, {});
kick(ArtilectName, NumParticles) when NumParticles >= 2, is_integer(NumParticles) ->
	%% Each new particle has it's own name to enable message passing via PubSub
	ParticleName = list_to_atom(atom_to_list(ArtilectName) ++ integer_to_list(NumParticles)), %% TODO I bet this is slow...
	particle:kickstart(ParticleName, {}),
	kick(ArtilectName, NumParticles-1). % Note: Must pass ArtilectName again, not ParticleName


%%% Used to create new particles in the PSO
new_particle(ArtilectName, ParticleNum) ->
	%% Each new particle has it's own name (i.e. unique number after it) to enable message passing via PubSub
	ParticleName = list_to_atom(atom_to_list(ArtilectName) ++ integer_to_list(ParticleNum)), %% TODO I bet this is slow...
	%% Particles are the things that fly around the swarm, which eventually get optimized through PSO
	ParticlePiD = particle:new(ParticleName, {}),
	% io:format("particle PID? ~p~n", [ParticlePiD]),
	% particle:initialize(ParticlePiD, ParticleNum),
	ok.