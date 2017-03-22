%%====================================================================
%%
%% @author Luke Taylor
%%
%%====================================================================
-module(particle).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% API
-export([start_link/1, stop/0]).	%% Required by gen_server
-export([initialize/2,				%% Sets up particle to get ready to swarm
		new_config/2,				%% Update the NN config,
		new_velocity/2,				%% Update velocity
		cycle/1,					%% This kickstarts the cycle (comes from exoself). Run against the scape, get results, report back to pso coordinator
		cycle_two/2,				%% Second step in the cycle process where we get input from the scape... there's a better way to do this for sure
		adjustment/2,				%% When a particle has reported values but needs to be updated, we call adjustment
		kickstart/2,					%% When a particle is first started it is given random values and then cycles
		new/2
		]).


-export([cons_state/1, cons_state_2/1]). %% EXPERIMENTAL FUNCTIONS

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {name, lobetype, testCount, pBest, pBestConfig, config, results, bias, velocity}).


-define(macro1(X, Y), {a, X, b, Y}).

%%%=============================================================================
%%% API functions
%%%=============================================================================


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API

new(ParticleName, DontKnow) -> %%% WTF...
	particle_sup:spawn_particle(ParticleName, DontKnow).

%%% Construct a net
initialize(CortexPid, ParticleNum) ->
	% io:format("CortexPid ~p~n", [CortexPid]),
	gen_server:cast(CortexPid, {initialize, ParticleNum}).

%%% Construct a net
new_config(CortexPid, NewConfig) ->
	gen_server:cast(CortexPid, {newConfig, NewConfig}).

%%% Construct a net
new_velocity(CortexPid, NewVelocity) ->
	gen_server:cast(CortexPid, {newVelocity, NewVelocity}).

%%% When a particle is first started it is given random values and then cycles
%%% Particles, when created, are subscribed to a pub/sub channel same as their ParticleName, thus how we send them messages
kickstart(ParticleName, Config) when Config == {} ->
	%% Default configuration
	% NN_Config = {{{0.5}, {0.5}}, {{0.5, 0.5}, {0.5, 0.5}, {0.5, 0.5}, {0.5, 0.5}}, {{0.5, 0.5, 0.5, 0.5}, {0.5, 0.5, 0.5, 0.5}}}, %% TODO make this not hard-coded
	% NN_Config = {sensor_layer(), middle_layer(), act_layer()},
	NN_Config = {{[n()], [n()]},
				 {[n(), n()], [n(), n()], [n(), n()], [n(), n()]},
				 {[n(), n(), n()]}},
	Bias = {{[n()], [n()]}, {[n()], [n()], [n()], [n()]}, {[n()]}},
	% NN_Config = {10}, %% Parabola
	% {InitialConfig, Velocity} = {NN_Config, {0.05}}, %% TODO random seed
	Velocity = {{{[0.01], [0.01]}, {[0.01, 0.01], [0.01, 0.01], [0.01, 0.01], [0.01, 0.01]}, {[0.01, 0.01, 0.01]}},
				% {{[n()], [n()]}, {[n()], [n()], [n()], [n()]}, {[n()]}}},
				{{[0.00], [0.00]}, {[0.00], [0.00], [0.00], [0.00]}, {[0.00]}}
				},
	ebus:pub(ParticleName, {newConfig,   NN_Config}),
	ebus:pub(ParticleName, {newBias,   Bias}),
	ebus:pub(ParticleName, {newVelocity, Velocity}),
	%ebus:pub(ParticleName, {cycle}).
	particle:cycle(ParticleName).
%%% Pre-defined starting config
% kickstart(ParticleName, Config) ->
% 	{{config, InitialConfig}, {velocity, Velocity}} = Config,
% 	ebus:pub(ParticleName, {newConfig,   InitialConfig}),
% 	ebus:pub(ParticleName, {newVelocity, Velocity}),
% 	ebus:pub(ParticleName, {cycle}).

%%% Cycle causes a particle to continue it's calculation/exploration for a while before reporting back
cycle(ParticleName) ->
	% io:format("CYCLE call: pid = ~p~n", [self()]),
	ebus:pub(ParticleName, {cycle}),
	ok.

%%% Second step in the cycle process where we get input from the scape... there's a better way to do this for sure
cycle_two(ParticleName, ScapeResults) ->
	ebus:pub(ParticleName, {cycle_two, ScapeResults}),
	ok.

%%% Adjustment
adjustment(ParticleName, {adjustment, GBest, BestConfig}) ->
	% io:format("ADJUSTMENT call: pid = ~p~n", [self()]),
	ebus:pub(ParticleName, {adjustment, GBest, BestConfig}),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Required by gen_server
%%% start link
start_link(CortexName) ->
	%% io:format("cortex name: ~p~n",[ArtilectName]),
	%% Explicitely not assigning an ID to prevent 'already registered errors'
	%% as multiple orbitos will spawn this module.
	gen_server:start_link(?MODULE, [CortexName], []).


%%% stop
stop() ->
	%% Stop server asynchronously
	io:format("Stopping~n"),
	gen_server:cast(?MODULE, shutdown).


%%% init
init([CortexName]) ->
	%% io:format("cortex Initializing with ~p~n", [ArtilectName]),
	%% process_flag(trap_exit, true),
	NewState = cons_state(CortexName),
	{ok, NewState}.


%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================


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
%%% Build a NN
handle_cast({initialize, ParticleNum}, State) ->
	%NetName = atom_to_list(State#state.name) ++ atom_to_list(ParticleNum),
	NetName = State#state.name,
	%NewState = State#state{name = NetName},
	% io:format("NETNAME: ~p NEWSTATE: ~p~n", [NetName, NewState]),

	{noreply, State};
%%% generic async handler
handle_cast(_Message, State) ->
	% io:format("Generic cast handler: '~p' while in '~p'~n", [Message, State]),
	{noreply, State}.
	%% Asynchronous, possible return values
	%% {noreply,NewState} | {noreply,NewState,Timeout} | {noreply,NewState,hibernate} | {stop,Reason,NewState}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% handle_info

handle_info({adjustment, GBest, BestConfig}, State) -> % TODO this adjustment should be calculated from GBest and BestConfig!
	% Delta = BestConfig - State#state.config,
	% io:format("DEBUG: State-config: ~p~n", [State#state.config]),
	% OldConfig = State#state.config,
	% V = State#state.velocity,
	% io:format("ADJUST HANDLER: pid = ~p~n", [self()]),
	NewState = calc_new_velocity(State, BestConfig),
	%NewConfig = {OldConfig + V},
	%NewState = State#state{config = NewConfig},
	NewV = NewState#state.velocity,
	% io:format("NEW V: ~p~n", [NewV]),


	% if
	% 	State#state.name == karly3 ->
	% 		%io:format("--- Update:: count: ~p gBest-Error ~p bestConfig ~p~n", [NewCount, State#state.gBest, State#state.bestConfig]);
	% 		io:format("NAME: ~p OldConfig: ~p OldVelocity: ~p NewVelocity: ~p~n", [State#state.name, State#state.config, State#state.velocity, NewState#state.velocity]);
	% 	true ->
	% 		pass
	% end,

	% io:format("Adjustment: OldV: ~p NewV ~p~n", [State#state.velocity, NewV]),

	% 
	% io:format("NAME: ~p~n OldConfig: ~p~n OldVelocity: ~p~n NewVelocity: ~p~n", [State#state.name, State#state.config, State#state.velocity, NewState#state.velocity]),
	%% Need to adjust the particle using the GBest and BestConfig  %% TODO Calculate new velocity based on pBest

	%% TODO Call cycle again with new configs
	particle:cycle(NewState#state.name),
	{noreply, NewState};
handle_info({cycle}, State) ->
	% io:format("CYCLE HANDLER: pid = ~p~n", [self()]),
	% io:format("CYCLE~n"),
	%% Get the particle to see how it performs on the scape & report back
	%OldConfig = State#state.config,
	%{V} = State#state.velocity,
	% NewConfig = config_update(State#state.config, State#state.velocity),
	% NewConfig2 = config_update2(OldConfig, V),
	% NewState = State#state{config = NewConfig},
	%% 1: Test particle & wait for result
	%io:format("H"),
	% io:format("Name: ~p OldConfig: ~p~n", [State#state.name, State#state.config]),

	NewState = apply_new_velocity_to_current_config(State),
	% io:format("PARTICLE - CYCLE - NewSTATE => ~p~n", [NewState]),

	% io:format("PARTCLE 223 - New Vel?? ~p~n", [NewState#state.velocity]),


	% if
	% 	State#state.name == karly3 ->
	% 		%io:format("--- Update:: count: ~p gBest-Error ~p bestConfig ~p~n", [NewCount, State#state.gBest, State#state.bestConfig]);
	% 		io:format("Name: ~p NewConfig: ~p~n", [NewState#state.name, NewState#state.config]);
	% 	true ->
	% 		pass
	% end,

	%io:format("OldConfig: ~p~n OldBias: ~p~n VELOCITY: ~p~n NewConfig: ~p~n NewBias: ~p~n", [State#state.config, State#state.bias, State#state.velocity, NewState#state.config, NewState#state.bias]),


	% io:format("Name: ~p NewConfig: ~p~n", [NewState#state.name, NewState#state.config]),
	run_tests(NewState), %% Shoot particle data off to scape and go back into loop to await results
	{noreply, NewState};
handle_info({cycle_two, Results}, State) ->
	%% 2: Report results back to PSO controller

	% io:format("Particle ~p Cycle2 - Res: ~p Count: ~p~n", [State#state.name, Results, State#state.testCount]),
    NewCount = State#state.testCount + 1,
    % %% Add results to results cache

    %% SOURCE OF BIG ERROR: Need some way to track when all different cortexes have finished that's not just counting cycles
    case NewCount of
        4 ->
            %% Get overall results by adding up all things in cache
            % io:format("Got 4 Results! Done!~n"),
            NewResultsList = cache_results(State, Results),
            FinalRes = add_results(NewResultsList),
            
            if
				%% First particle to report back -> set initial values
				State#state.pBest == 'unset' ->
					NewState = State#state{testCount = 0, pBestConfig = State#state.config, pBest = FinalRes, results = []};
				(State#state.pBest /= 'unset') and (State#state.pBest < FinalRes) -> 
					NewState = State#state{testCount = 0, results = []};
				%% Update pBest
				(State#state.pBest /= 'unset') and (State#state.pBest >= FinalRes) ->
					NewState = State#state{testCount = 0, pBestConfig = State#state.config, pBest = FinalRes, results = []}
			end,

            % {ActuatorOutput, ExpectedOutput, Error} = Results,
            %% TODO Need to figure out a way of reporting back without the NN name being hard coded
			pso:particle_checkin({karly, {{pso_update, NewState#state.name}, NewState#state.config, NewState#state.velocity, FinalRes}}); %% NEED TO AGGREGATE RESULTS
        _Else ->
        	%{ActuatorOutput, ExpectedOutput, Error} = Results,
        	NewResultsList = cache_results(State, Results),
        	% io:format("PARTICLE257 - NewResultsList = ~p~n", [NewResultsList]),
			% Error = calc_error(ExpectedOutput, ActuatorOutput),
			% io:format("INPUT: ~p ERROR: ~p Scape ~p~n", [Inputs, Error, State#state.name]),
            NewState = State#state{results = NewResultsList, testCount = NewCount}
    end,
	{noreply, NewState};


%%% IDEA: When we add up returns from all the scapes, keep an 'uncertainty' accumulator.
%%% We can derive the uncertainty of our guess baased on our test results

handle_info({newVelocity, NewVelocity}, State) ->
	NewState = State#state{velocity = NewVelocity},
	{noreply, NewState};
handle_info({newBias, NewBias}, State) ->
	NewState = State#state{bias = NewBias},
	{noreply, NewState};
handle_info({newConfig, NewConfig}, State) ->
	NewState = State#state{config = NewConfig},
	{noreply, NewState};
handle_info(_Message, _State) ->
	% io:format("Generic info handler: '~p' '~p'~n", [_Message, _State]),
	{noreply, _State}.
	%% Informative calls - possible return values
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


%%% number of input links
n() -> trunc(rand:uniform(), 2).

neuron(1) ->
	[n()];
neuron(2) ->
	[rand:uniform(), rand:uniform()];
neuron(3) ->
	[rand:uniform(), rand:uniform(), rand:uniform()].

sensor_layer() ->
	{[neuron(1), neuron(1)]}.

middle_layer() ->
	{[neuron(2), neuron(2), neuron(2)]}.

act_layer() ->
	{[neuron(3)]}.





add_results(ResultsList) ->
	{ActuatorOutput1, ExpectedOutput1, Error1} = lists:nth(1, ResultsList),
	{ActuatorOutput2, ExpectedOutput2, Error2} = lists:nth(2, ResultsList),
	{ActuatorOutput3, ExpectedOutput3, Error3} = lists:nth(3, ResultsList),
	{ActuatorOutput4, ExpectedOutput4, Error4} = lists:nth(4, ResultsList),
	Error = ((Error1 + Error2 + Error3 + Error4)/4),
	% A_Out = {Error, {{ActuatorOutput1, ExpectedOutput1},
	% 				 {ActuatorOutput2, ExpectedOutput2},
	% 				 {ActuatorOutput3, ExpectedOutput3},
	% 				 {ActuatorOutput4, ExpectedOutput4}}},
	% io:format("AOuT: ~p~n", [A_Out]),
	A_Out = trunc(Error, 3),
	% io:format("4 RESULTS:~nA1: ~p A1-Exp: ~p~n", [ActuatorOutput1, ExpectedOutput1]),
	% io:format("A2: ~p A2-Exp: ~p~n", [ActuatorOutput2, ExpectedOutput2]),
	% io:format("A3: ~p A3-Exp: ~p~n", [ActuatorOutput3, ExpectedOutput3]),
	% io:format("A4: ~p A4-Exp: ~p~n", [ActuatorOutput4, ExpectedOutput4]),
	% io:format("ERROR: ~p~n", [A_Out]),
	A_Out.


cache_results(State, Results) ->
	% io:format("PARTICLE341 - Results: ~p~n", [Results]),
	ResultsList = [Results],
	NewList = State#state.results ++ ResultsList,
	NewList.


run_tests(State) ->
	Name = State#state.name,
	TestData = {State#state.config, State#state.bias},
	%% TODO spawn new scapes here
	% io:format("ParticleName: ~p~n TestData ~p~n", [Name, TestData]),
	% scape:test_particle(Name, {TestData, {0,0}, 0}),
	% scape:test_particle(Name, {TestData, {0,1}, 1}),
	% scape:test_particle(Name, {TestData, {1,0}, 1}),
	% io:format("Spawning a scape~n"),
	ScapeName1 = list_to_atom(atom_to_list(Name) ++ integer_to_list(1)), %% TODO I bet this is slow...
	% io:format("PARTICLE => Scape Name1: ~p~n", [ScapeName1]),
	_ScapePiD1 = scape:new(ScapeName1, {}), %% Create a scape for testing purposes
	scape:test_particle(ScapeName1, {TestData, {1,1}, 0}),

	ScapeName3 = list_to_atom(atom_to_list(Name) ++ integer_to_list(3)), %% TODO I bet this is slow...
	% io:format("PARTICLE => Scape Name1: ~p~n", [ScapeName1]),
	_ScapePiD3 = scape:new(ScapeName3, {}), %% Create a scape for testing purposes
	scape:test_particle(ScapeName3, {TestData, {1,0}, 1}),

	ScapeName4 = list_to_atom(atom_to_list(Name) ++ integer_to_list(4)), %% TODO I bet this is slow...
	% io:format("PARTICLE => Scape Name1: ~p~n", [ScapeName1]),
	_ScapePiD4 = scape:new(ScapeName4, {}), %% Create a scape for testing purposes
	scape:test_particle(ScapeName4, {TestData, {0,1}, 1}),

	ScapeName2 = list_to_atom(atom_to_list(Name) ++ integer_to_list(2)), %% TODO I bet this is slow...
	% io:format("PARTICLE => Scape Name2: ~p~n", [ScapeName1]),
	_ScapePiD2 = scape:new(ScapeName2, {}), %% Create a scape for testing purposes
	scape:test_particle(ScapeName2, {TestData, {0,0}, 0}).


calc_new_velocity(State, GBest) ->
	%%% https://visualstudiomagazine.com/articles/2013/12/01/neural-network-training-using-particle-swarm-optimization.aspx
	%%% v(t+1) = (Inertia * OldWeightVelocity) 	+ 
	%%%			 (c1 * r1 * (p(t) – x(t)) 	+
	%%%			 (c2 * r2 * (g(t) – x(t))
	VV = State#state.velocity,
	%io:format("Vel: ~p~n", [VV]),
	{OldWeightVelocity, OldBiasVelocity} = VV,
	% OldWeightVelocity = State#state.velocity,
	% io:format("PARTICLE LN#373 - Config: ~p~n", [State#state.config]),
	CC = State#state.config,
	%io:format("CC: ~p~n", [CC]),

 	% {
 	% 	{[0.678],[0.262]},
  %    	{[0.06,0.632],[0.386,0.335],[0.569,0.26],[0.683,0.603]},
  %    	{[0.477,0.271,0.073]}
  %   }


	% {OldWeighting, OldBias} = CC, %% GETS CALLED IF WE DO AN ADJUSTMENT
	OldWeighting = CC,
	OldBias = State#state.bias,
	% io:format("OLD BIAS!!! ~p~n", [OldBias]),

	Inertia = 0.05,
	R1 = rand:uniform(), R2 = rand:uniform(),
	LocalWeightFactor = 0.05, GlobalWeightFactor = 0.9,

	PBest = State#state.pBestConfig,
	 %io:format("PBEST = ~p~n", [PBest]),

	Subtract = fun(X, Y) -> %% TODO need description of FitnessFunc
				Return = trunc(((X - Y)), 3),
				Return
				end,

	Add = fun(X, Y) -> %% TODO need description of FitnessFunc
				Return = trunc(((X + Y)), 3),
				Return
				end,

	%%%%% WEIGHTINGS
	Precision = 3,

	% Term 1
	ScaleElements = fun(Y) -> trunc((Inertia * Y), Precision) end,
	FirstTermW = apply_to_vel_vector(OldWeightVelocity, ScaleElements),
	% Term 2
	if
		PBest /= 'unset' ->
			ScaleElements2 = fun(Y) -> trunc((LocalWeightFactor * R1 * Y), Precision) end,

			Next = combine_vel_vectors(PBest, OldWeightVelocity, Subtract),
			SecondTermW = apply_to_vel_vector(Next, ScaleElements2);
		true ->
			SecondTermW = {{[0], [0]}, {[0, 0], [0, 0], [0, 0], [0, 0]}, {[0, 0, 0]}}
	end,

	HalfWay = combine_vel_vectors(FirstTermW, SecondTermW, Add),


	ScaleElements3 = fun(Y) -> trunc((GlobalWeightFactor * R2 * Y), Precision) end,
	
	Next2 = combine_vel_vectors(GBest, OldWeightVelocity, Subtract),
	


	ThirdTermW = apply_to_vel_vector(Next2, ScaleElements3),

	FinalWeightings = combine_vel_vectors(HalfWay, ThirdTermW, Add),

	% io:format("GBest: ~p~n OldVelocity ~p~n NewVelocity: ~p~n", [GBest, OldWeightVelocity, FinalWeightings]),

	%%%%% BIAS


	%%% SUCH A BUG TODO - GET THIS WORKING


	% % Term 1
	% FirstTermB = apply_to_vel_bias_velocity(OldBiasVelocity, ScaleElements),
	% % Term 2
	% if
	% 	PBest /= 'unset' ->
	% 		ScaleElements2B = fun(Y) -> (LocalWeightFactor * R1 * Y) end,
			
	% 		NextB = combine_bias_vectors(PBest, OldBiasVelocity, Subtract),
	% 		SecondTermWB = apply_to_vel_vector(NextB, ScaleElements2B);
	% 	true ->
	% 		SecondTermWB = {{[0], [0]}, {[0, 0], [0, 0], [0, 0], [0, 0]}, {[0, 0, 0]}}
	% end,

	% HalfWayB = combine_vel_vectors(FirstTermB, SecondTermWB, Add),


	% ScaleElements3B = fun(Y) -> (GlobalWeightFactor * R2 * Y) end,
	
	% Next2B = combine_vel_vectors(GBest, OldBiasVelocity, Subtract),
	% ThirdTermWB = apply_to_vel_vector(Next2B, ScaleElements3B),

	% FinalBias = combine_vel_vectors(HalfWayB, ThirdTermWB, Add),








	% IdealConfig = {{[1.0], [1.0]}, {[1.0, 0.0], [0.5, 0.5], [0.0, 1.0], [0.0, 0.0]}, {[1.0, -1.0, 1.0]}},
	% IdealBias = {{[0.0], [0.0]}, {[0.0], [0.0], [0.0], [0.0]}, {[0.0]}},
	% {{[S1w1], [S2w1]}, {[N1w1, N1w2], [N2w1, N2w2], [N3w1, N3w2], [N4w1, N4w2]}, {[A1w1, A1w2, A1w3]}} = State#state.config,
	% {{[BestS1w1], [BestS2w1]},
	% {[BestN1w1, BestN1w2], [BestN2w1, BestN2w2], [BestN3w1, BestN3w2], [BestN4w1, BestN4w2]},
	% {[BestA1w1, BestA1w2, BestA1w3]}} = BestConfig,
	% SF = 0.5, % Scaling factor
	% NewVelocity = {{[(BestS1w1 - S1w1) * SF], [(BestS2w1 - S2w1) * SF]},
	% 			   {[(BestN1w1 - N1w1) * SF, (BestN1w2 - N1w2) * SF],
	% 			   		[(BestN2w1 - N2w1) * SF, (BestN2w2 - N2w2) * SF],
	% 			   			[(BestN3w1 - N3w1) * SF, (BestN3w2 - N3w2) * SF],
	% 			   				[(BestN4w1 - N4w1) * SF, (BestN4w2 - N4w2) * SF]},
	% 			   	{[(BestA1w1 - A1w1) * SF, (BestA1w2 - A1w2) * SF, (BestA1w3 - A1w3) * SF]}},
	% NewVelocity = combine_vectors_v_refactorMe(BestConfig, State#state.config),


	% BiasV2 = {{[0.0], [0.0]}, {[0.0], [0.0], [0.0], [0.0]}, {[0.0]}},
	% NewVelocity1 = combine_vectors_v_refactorMe(IdealConfig, WeightVelocity),
	

	% NewVelocity2 = {NewVelocity1, BiasV2},
	% FinaLVel = {FinalWeightings, FinalBias},
	FinaLVel = {FinalWeightings, OldBias}, %% TODO SAD!! :()

	% io:format("OldConfig: ~p OldV ~p BestConfig ~p~n", [OldConfig, OldVelocity, BestConfig]),
	NewState = State#state{velocity=FinaLVel},
	% io:format("NEWSTATE: Velocity: ~p~n", [NewState#state.velocity]),
	NewState.

apply_to_vel_vector(V1, Func) ->
	%io:format("V1: ~p pid = ~p~n", [V1, self()]),
	{{[V1a], [V1b]}, {[V1c, V1d], [V1e, V1f], [V1g, V1h], [V1i, V1j]}, {[V1k, V1l, V1m]}} = V1,
	%io:format("v1A: ~p~n", [V1a]),
	%io:format("RES: ~p~n", [Func(V1a)]),
	V3 = {{[Func(V1a)], [Func(V1b)]},
		 {[Func(V1c), Func(V1d)], [Func(V1e), Func(V1f)], [Func(V1g), Func(V1h)], [Func(V1i), Func(V1j)]},
		 {[Func(V1k), Func(V1l), Func(V1m)]}},
	V3.

apply_to_vel_bias_velocity(Bias, Func) ->
	{{[B1], [B2]}, {[B3], [B4], [B5], [B6]}, {[B7]}} = Bias,
	V3 = {{[Func(B1)], [Func(B2)]}, {[Func(B3)], [Func(B4)], [Func(B5)], [Func(B6)]}, {[Func(B7)]}},
	V3.

combine_vel_vectors(V1, V2, Func) ->
	%io:format("COMBINE VEL VECT - V1: ~p pid = ~p~n", [V1, self()]),
	{{[V1a], [V1b]}, {[V1c, V1d], [V1e, V1f], [V1g, V1h], [V1i, V1j]}, {[V1k, V1l, V1m]}} = V1,
	{{[V2a], [V2b]}, {[V2c, V2d], [V2e, V2f], [V2g, V2h], [V2i, V2j]}, {[V2k, V2l, V2m]}} = V2,
	V3 = {{[Func(V1a, V2a)], [Func(V1b, V2b)]},
		 {[Func(V1c, V2c), Func(V1d, V2d)], [Func(V1e, V2e), Func(V1f, V2f)], [Func(V1g, V2g), Func(V1h, V2h)], [Func(V1i, V2i), Func(V1j, V2j)]},
		 {[Func(V1k, V2k), Func(V1l, V2l), Func(V1m, V2m)]}},
	V3.


combine_bias_vectors(Bias, Bias2, Func) ->
	% io:format("COMBINE VEL VECT - V1: ~p pid = ~p~n", [V1, self()]),
	{{[B1a], [B1b]}, {[B1c], [B1d], [B1e], [B1f]}, {[B1g]}} = Bias,
	{{[B2a], [B2b]}, {[B2c], [B2d], [B2e], [B2f]}, {[B2g]}} = Bias2,
	V3 = {{[Func(B1a, B2a)], [Func(B1b, B2b)]}, {[Func(B1c, B2c)], [Func(B1d, B2d)], [Func(B1e, B2e)], [Func(B1f, B2f)]}, {[Func(B1g, B2g)]}},
	V3.



combine_vectors_v_refactorMe(V1, V2) ->
	F = 1,
	Func = fun(X, Y) -> %% TODO need description of FitnessFunc
		Return = trunc(((X - Y) * F), 3),
		Return
		end,
	{{[V1a], [V1b]}, {[V1c, V1d], [V1e, V1f], [V1g, V1h], [V1i, V1j]}, {[V1k, V1l, V1m]}} = V1,
	{{[V2a], [V2b]}, {[V2c, V2d], [V2e, V2f], [V2g, V2h], [V2i, V2j]}, {[V2k, V2l, V2m]}} = V2,
	V3 = {{[Func(V1a, V2a)], [Func(V1b, V2b)]},
		 {[Func(V1c, V2c), Func(V1d, V2d)], [Func(V1e, V2e), Func(V1f, V2f)], [Func(V1g, V2g), Func(V1h, V2h)], [Func(V1i, V2i), Func(V1j, V2j)]},
		 {[Func(V1k, V2k), Func(V1l, V2l), Func(V1m, V2m)]}},
	V3.

% combine_vectors(V1, V2, Func) ->
% 	{{[V1a], [V1b]}, {[V1c, V1d], [V1e, V1f], [V1g, V1h], [V1i, V1j]}, {[V1k, V1l, V1m]}} = V1,
% 	{{[V2a], [V2b]}, {[V2c, V2d], [V2e, V2f], [V2g, V2h], [V2i, V2j]}, {[V2k, V2l, V2m]}} = V2,
% 	V3 = {{[Func(V1a, V2a)], [Func(V1b, V2b)]},
% 		 {[Func(V1c, V2c), Func(V1d, V2d)], [Func(V1e, V2e), Func(V1f, V2f)], [Func(V1g, V2g), Func(V1h, V2h)], [Func(V1i, V2i), Func(V1j, V2j)]},
% 		 {[Func(V1k, V2k), Func(V1l, V2l), Func(V1m, V2m)]}},
% 	V3.

add_bias_velocity(Bias, Velocity) ->
	{{[B1], [B2]}, {[B3], [B4], [B5], [B6]}, {[B7]}} = Bias,
	{{[B1v], [B2v]}, {[B3v], [B4v], [B5v], [B6v]}, {[B7v]}} = Velocity,
	V3 = {{[B1 + B1v], [B2 + B2v]}, {[B3 + B3v], [B4 + B4v], [B5 + B5v], [B6 + B6v]}, {[B7 + B7v]}},
	V3.

%%% Add velocity to vector
apply_new_velocity_to_current_config(State) ->
	% io:format("V: ~p", [State#state.velocity]),
	{WeightVelocity, BiasVelocity} = State#state.velocity,
	% {{[S1w1], [S2w1]}, {[N1w1, N1w2], [N2w1, N2w2], [N3w1, N3w2], [N4w1, N4w2]}, {[A1w1, A1w2, A1w3]}} = State#state.config,
	% {{[S1v1], [S2v1]}, {[N1v1, N1v2], [N2v1, N2v2], [N3v1, N3v2], [N4v1, N4v2]}, {[A1v1, A1v2, A1v3]}} = State#state.velocity,
	% NewConfig = {{[S1w1 + S1v1], [S2w1 + S2v1]},
	% 			{[N1w1 + N1v1, N1w2 + N1v2], [N2w1 + N2v1, N2w2 + N2v2], [N3w1 + N3v1, N3w2 + N3v2], [N4w1 + N4v1, N4w2 + N4v2]},
	% 			{[A1w1 + A1v1, A1w2 + A1v2, A1w3 + A1v3]}},
	Func = fun(X, Y) -> trunc((trunc(X, 3) + trunc(Y, 3)), 3) end,

	% io:format("Config: ~p WeightV: ~p~n", [State#state.config, WeightVelocity]),

	NewConfig = combine_vel_vectors(State#state.config, WeightVelocity, Func),
	%NewBias = combine_vectors_bias(State#state.bias, BiasVelocity, Func),
	 %NewBias = add_bias_velocity(State#state.bias, BiasVelocity), %% TODO REMOVE I THINK
	% NewBias = combine_bias_vectors(State#state.bias, BiasVelocity, Func),
	NewBias = State#state.bias, %% TODO FIX THIS SHOULD USE LINE ABOVE

	NewState = State#state{config = NewConfig, bias = NewBias},
	% io:format("OldConfig: ~p NewConfig: ~p~n", [State#state.config, NewState#state.config]),
	NewState.



%%% uses the velocity to update the current config
config_update(OldConfig, Velocity) ->
	%%% TODO but for now just keep adding to it :P
	{{A, B}, {C, D, E, F}, {G, H}} = OldConfig,
	{{trunc(A+0.05, 3), trunc(B+0.05, 3)}, {trunc(C+0.05, 3), trunc(D+0.05, 3), trunc(E+0.05, 3), trunc(F+0.05, 3)}, {trunc(G+0.05, 3), trunc(H+0.05, 3)}}.

%%% uses the velocity to update the current config
config_update2(OldConfig, Velocity) -> %% Parabola update
	%%% TODO but for now just keep adding to it :P
	% io:format("OldC: ~p V ~p~n", [OldConfig, Velocity]),
	{X} = OldConfig,
	V = Velocity,
	{X + V}.


%%% Truncates a floating point number F to N decimal places ignoring trailing zeros.
%%% http://www.qlambda.com/2013/10/erlang-truncate-floating-point-number.html
-spec trunc(float()|integer(), integer()) -> float().
trunc(Num, NumDigits) ->
	if
		Num < 0.0001 ->
			Return = Num;
		true ->
			Prec = math:pow(10, NumDigits),
			% io:format("pid = ~p NUM: ~p PREC: ~p~n", [self(), Num, Prec]),
    		Return = trunc(Num*Prec)/Prec
	end,
	Return.
    



cons_state(CortexName) ->
	% io:format("Constructing State - PARTICLE~n"),
	S = #state{
			  name = CortexName,
		  lobetype = particle,
		 testCount = 0,
			config = [],
			 pBest = unset,
	   pBestConfig = {},
			  bias = [],
		   results = [],
		  velocity = {unset}
		},
	S.


%%%%%% THIS IS VERY COOL

cons_state_2(_Options) -> fun(Action, Data) ->
	
	%%% State Variables
	ClassFields = [{ceo, 'rory'}],
	% lists:append(ClassFields, {coo, 'luke'}),
	% lists:append(ClassFields, {cto, 'brendan'}),

	%%% Getters & Setters
	case Action of
        get ->
        	case lists:keyfind(Data, 1, ClassFields) of
        		{Data, Result} -> Result;
        		false -> nothing
    		end;

        toString ->
        	io:format("ClassFields: ~p~n", [ClassFields])
    end



	end. % END cons_state_2


