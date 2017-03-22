-module(scape).
-behaviour(gen_server).
%%%=============================================================================
%%% Particle Swarm Optimization module
%%%
%%% @author Luke Taylor
%%% @doc This module, the pso controller, implements particle swarm optimization in parallel
%%% Rather than keep track of PiDs and doing messaing this way, ebus - a pub/sub application is used
%%%=============================================================================


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Header information
%%% API Exports
-export([start_link/1, stop/0]).
-export([test_particle/2,            %% Particle sends it's data, and scape tests it
         return_results/2,            %% Cortex calls this to return results to the scape
         new/2
        ]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% State variable
-record(state, {name, numSims, testData, simCount}).

%%% Macros
-define(SERVER, ?MODULE).


%%%=============================================================================
%%% --- API functions
%%%=============================================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API


%%% Test the particle config against the simulation scape
test_particle(ScapeName, {ParticleData, Input, ExpectedOutput}) ->
    %% ScapeName should be same as ParticleName, and both are subscribed to that pub/sub channel
    ebus:pub(ScapeName, {scape, test, {ParticleData, Input, ExpectedOutput}}). %% TODO Make this an API function of Scape


%%% The cortex returns results here
return_results(ScapeName, Results) ->
    {_Inputs, _ActuatorOutput, _ExpectedOutput} = Results, %% Pattern matching check
    % io:format("RESULTS RETURNED~n SCAPE NAME ~p RESULTS ~p~n~n", [ScapeName, Results]),
    ebus:pub(ScapeName, {scape, results, Results}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Required by gen_server
%%% start link
start_link(Params) ->
    %% io:format("scape name: ~p~n",[Params]),
    %% Explicitely not assigning an ID to prevent 'already registered errors'
    %% as multiple orbitos will spawn this module.
    gen_server:start_link(?MODULE, [Params], []).

%%% stop
stop() ->
	%% Stop server asynchronously
    io:format("Stopping~n"),
    gen_server:cast(?MODULE, shutdown).


new(Name, _DontKnow) -> %5 DontKnow? DONTKNOW!?!?!?
    scape_sup:spawn_scape(Name, _DontKnow).


%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% init
init([Params]) ->
    % io:format("scape Initializing with ~p~n", [Params]),
    %% process_flag(trap_exit, true),
    NewState = #state{name = Params,
                   numSims = unset,
                  testData = unset,
                  simCount = 0},
    {ok, NewState}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% handle_call
handle_call(Message, From, State) ->
    io:format("Generic call handler: '~p' from '~p' while in '~p'~n", [Message, From, State]),
    {reply, ok, State}.
    %% Synchronous, possible return values
    %% {reply, Reply, NewState} | {reply, Reply, NewState, Timeout} | {reply, Reply, NewState, hibernate} | {noreply, NewState} |
    %% {noreply, NewState, Timeout} | {noreply, NewState, hibernate} |  {stop, Reason, Reply, NewState} | {stop, Reason, NewState}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% handle_cast
%%% normal termination clause
handle_cast(shutdown, State) ->
    io:format("Generic cast handler: *shutdown* while in '~p'~n", [State]),
    {stop, by_request, State};
%% generic async handler
handle_cast(Message, State) ->
    io:format("Generic cast handler: '~p' while in '~p'~n", [Message, State]),
    {noreply, State}.
    %% Asynchronous, possible return values
    %% {noreply, NewState} | {noreply, NewState, Timeout} | {noreply, NewState, hibernate} | {stop, Reason, NewState}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% handle_info
%%% Test inputs on the scape
handle_info({scape, test, {ParticleData, Input, ExpectedOutput}}, State) ->
    %% TODO Add checking to see if less than previous outputs (pBest)

    % io:format("Scape ~p testing data: ~p on Input ~p~n", [State#state.name, ParticleData, Input]),
    % Get simulation output
    % parabola(State, ParticleData),
    %% Update Net with new weightings
    xorr_neural(State, {ParticleData, Input, ExpectedOutput}), % Now await cast back from Cortex with PiD to run tests
    {noreply, State};
%%% Return results to particle
handle_info({scape, results, Results}, State) ->
    %% Get results back to Particle
    %% TODO Check that all parallel cortexes running independent tests have finished
    % io:format("Got results!! NAME: ~p RES: ~p~n", [State#state.name, Results]),
    {Inputs, RealOutput, DesiredOutput} = Results,
    Error = trunc(abs(DesiredOutput - RealOutput), 4),
    % io:format("SCAPE Inputs: ~p RealOut: ~p Desired: ~p ERROR ~p~n", [Inputs, RealOutput, DesiredOutput, Error]),
    FinalRes = {RealOutput, DesiredOutput, Error},
    ParticleName = list_to_atom(return_particle_name(atom_to_list(State#state.name))),
    % io:format("PName: ~p~n", [ParticleName]),
    particle:cycle_two(ParticleName, FinalRes), %% NEED TO STRIP SCAPE NAME TO SEND BACK TO PARTICLE

    % io:format("Adding 1 to simCOunt"),
    % NewCount = State#state.simCount + 1,
    % %% Add results to results cache

    % %% SOURCE OF BIG ERROR: Need some way to track when all different cortexes have finished that's not just counting cycles
    % case NewCount of
    %     2 ->
    %         %% Get overall results by adding up all things in cache
    %         % io:format("Got 2 Results! Done!"),
    %         NewState = State#state{simCount = 0},
    %         particle:cycle_two(NewState#state.name, Results);
    %     _Else ->
    %         NewState = State#state{simCount = NewCount}
    % end,


    % {noreply, NewState};
    {stop, normal, State}; %% Finished with this scape
%%% generic info handler
handle_info(_Message, _State) ->
    % io:format("Generic info handler: '~p' '~p'~n", [_Message, _State]),
    {noreply, _State}.
    %% Informative calls
    %% {noreply, NewState} | {noreply, NewState, Timeout} | {noreply, NewState, hibernate} |
    %% {stop, Reason, NewState} | {stop, Reason, NewState}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% server termination
terminate(_Reason, _State) ->
    % io:format("Generic termination handler: '~p' '~p'~n", [_Reason, _State]).
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% code change
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================


%%% TODO We're well passed judgement at this stage, clean it up later
return_particle_name(ScapeName) ->
    %%% Strip last character
    ListLength = length(ScapeName),
    PName = lists:sublist(ScapeName, (ListLength-1)),
    PName.


%%% Truncates a floating point number F to N decimal places ignoring trailing zeros.
%%% http://www.qlambda.com/2013/10/erlang-truncate-floating-point-number.html
-spec trunc(float()|integer(), integer()) -> float().
trunc(Num, NumDigits) ->
    Prec = math:pow(10, NumDigits),
    trunc(Num*Prec)/Prec.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scape simulation functions
%%% These functions return the number of sims to be run, then cause a cast for each sim to be sent
%%% back to this gen_server. Once all the sims have run, the scape collated their collective results
%%% xor
xorr({A, B}) ->
    case {A, B} of
        {0, 0} -> 0;
        {0, 1} -> 1;
        {1, 0} -> 1;
        {1, 1} -> 0;
         _Else ->
            false
    end.

%%% Inverted parabola
parabola(State, Xin) ->
    %% Global max at X = -(23/30)
    {X} = Xin,
    % Y = (-15)*X*X - 23*X + 7,
    Y = -28*X*X+6*X-17,
    %% Return scape results to Particle
    io:format("SCAPE - ParabolaFunc: Input: ~p Output ~p~n", [Xin, Y]),
    particle:cycle_two(State#state.name, Y).

%%% Test out an input config on the cortex/NN. Cortex will cast results back to this scape
xorr_neural(State, {Input_NN_Config, Input, ExpectedOutput}) ->
    %% Send new config to Cortex
    {NN, Bias} = Input_NN_Config,
    % io:format("SCAPE: Bias: ~p~n", [Bias]),
    % io:format("XORR_neural: Name: ~p Input ~p InputConfig: ~p ExpOut ~p~n", [State#state.name, Input, InputConfig, ExpectedOutput]),
    
    %% Trigger cortex to start cycling through test data
    %% TODO This is where a parallel call should be made
    % io:format("Spawning Neurons~n"),
    % NN_Config = {{[0.5], [0.5]}, {[0.5, 0.5], [0.5, 0.5], [0.5, 0.5], [0.5, 0.5]}, {[0.5, 0.5, 0.5, 0.5]}}, %% TODO make this not hard-coded
    %% First test
    cortex:new(State#state.name, Input_NN_Config),
    % cortex:construct_net(State#state.name, InputConfig),
    %% Send inputs to Cortex's
    cortex:sensor_input(State#state.name, {Input, ExpectedOutput}).
    % cortex:finish(State#state.name),

    % %% Second test
    % cortex_sup:spawn_cortex(State#state.name, {}),
    % cortex:construct_net(State#state.name), %% TODO SHOULD USE CONFIG
    % %% Send inputs to Cortex's
    % cortex:sensor_input(State#state.name, {1, 1}).

    % io:format("Spawning Neurons~n"),
    %CortexPiD2 = cortex_sup:spawn_cortex(State#state.name, {}),
   %  cortex:construct_net(CortexPiD2),

    % io:format("Spawning Neurons~n"),
    %CortexPiD3 = cortex_sup:spawn_cortex(State#state.name, {}),
    % cortex:construct_net(CortexPiD3),

    % io:format("Spawning Neurons~n"),
    %CortexPiD4 = cortex_sup:spawn_cortex(State#state.name, {}),
    % cortex:construct_net(CortexPiD4),

    % cortex:update_weightings(State#state.name, InputConfig),

    % Result1 = gen_server:call(CortexPiD1, {test_nn_config, {0, 0}}),
    % Result2 = gen_server:call(CortexPiD2, {test_nn_config, {0, 1}}),
    % Result3 = gen_server:call(CortexPiD3, {test_nn_config, {1, 0}}),
    % Result4 = gen_server:call(CortexPiD4, {test_nn_config, {1, 1}}),


    

    % FinalRes = ((Result1 + Result2 + Result3 + Result4) / 4),

     %%particle:cycle_two(State#state.name, 10).


%%% MNIST Lookup

%%% CIPHER10 Lookup

%%% Stockbroker

%%% Go player

%%% User-defined -> TODO Implement a scape behaviour