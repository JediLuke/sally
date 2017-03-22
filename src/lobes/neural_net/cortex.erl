%%====================================================================
%%
%% @author Luke Taylor
%%
%%====================================================================
-module(cortex).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% API
-export([start_link/3, stop/0]).	%% Required by gen_server
-export([%construct_net/2,           %% Spawns required processes for a basic NN
		 update_weightings/2,		%% Update weightings of neurons
		 actuator_output/2,			%% Handle output data from an actuator
		 finish/1,
		 sensor_input/2,				%% Allows us to send the cortex input which triggers some computation
		 new/2
		]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {name, lobetype, weightings, bias, inputs, expectedOutput, syncPid}).


%%%=============================================================================
%%% API functions
%%%=============================================================================

new(Name, NN_Config) ->
	cortex_sup:spawn_cortex(Name, NN_Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API
%%% Construct a net
% construct_net(CortexName, InputConfig) ->
% 	% io:format("CortexPid ~p~n", [CortexPid]),
% 	% gen_server:cast(CortexPid, {makeNet}).
% 	ebus:pub(CortexName, {makeNet, InputConfig}).


%%% Update Weightings
update_weightings(CortexName, Weightings) ->
	% io:format("Cortex: Update weightings - CortexName ~p Weightings ~p~n", [CortexName, Weightings]),
	% gen_server:cast(CortexName, {new_weightings, Weightings}).
	ebus:pub(CortexName, {new_weightings, Weightings}).


%%% Test out an input on the Cortex/NN
sensor_input(CortexPid, {Inputs, ExpectedOutput}) ->
	% io:format("CortexPid ~p Inputs: ~p~n", [CortexPid, Inputs]),
	% gen_server:cast(CortexPid, {ctx_inputs, Inputs}).
	ebus:pub(CortexPid, {sensor_inputs, {Inputs, ExpectedOutput}}).


%%% Handle output data from an actuator
actuator_output(CortexName, {ActuatorOutput}) ->
	% io:format("CortexPid ~p~n", [CortexPid]),
	% gen_server:cast(CortexPid, {ctx_inputs, Inputs}).
	ebus:pub(CortexName, {actuator_output, {ActuatorOutput}}).


%%% End the simulation
finish(CortexName) ->
	ebus:pub(CortexName, {finish}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Required by gen_server
%%% start link
start_link(CortexName, NN_Config, BiasVector) ->
	%% io:format("cortex name: ~p~n",[ArtilectName]),
	%% Explicitely not assigning an ID to prevent 'already registered errors'
	%% as multiple orbitos will spawn this module.
	gen_server:start_link(?MODULE, [CortexName, NN_Config, BiasVector], []).

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
init([CortexName, NN_Config, BiasVector]) ->
	% io:format("Cortex Initializing with name: ~p~n", [CortexName]),
	%% process_flag(trap_exit, true),
	NewState = #state{name = CortexName,
				  lobetype = cortex,
				  weightings = NN_Config,
				  	 bias = BiasVector,
				  syncPid = unset,
				   inputs = unset,
		   expectedOutput = unset},
	ok = build_net(NewState),
	{ok, NewState}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% handle_call
handle_call({test_nn_config, TestData}, From, State) ->
	% io:format("Cortex testing Data: ~p on NN config: ~p~n", [TestData, State#state.weightings]),
	%% TODO Call to Neurons
	Result = gen_server:call(State#state.syncPid, {sync, TestData}),
	{reply, Result, State};
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
handle_cast(_Message, State) ->
	% io:format("Generic cast handler: '~p' while in '~p'~n", [Message, State]),
	{noreply, State}.
	%% Asynchronous, possible return values
	%% {noreply,NewState} | {noreply,NewState,Timeout} | {noreply,NewState,hibernate} | {stop,Reason,NewState}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% handle_info
%%% Build a NN
handle_info({makeNet, InputConfig}, State) ->
	%NetName = atom_to_list(State#state.name) ++ atom_to_list(ParticleNum),
	% io:format("Spawning Neurons~n"),
	% NN_Config = {2, 3, 1}, %% TODO make this not hard-coded
	% NN_Config = InputConfig,
	_NewState = build_net(State),

	{noreply, State};
%%% update weightings
handle_info({new_weightings, Weightings}, State) ->
	% io:format("CTX Cast: New Weights: Name: ~p Weightings ~p~n", [State#state.name, Weightings]),
	%NetName = atom_to_list(State#state.name) ++ atom_to_list(ParticleNum),
	%% TEST - 2 x 4 x 1
	% {{A, B}, {C, D, E, R}, {F, G}} = Weightings,
	%  io:format("Sample weights: ~p ~p ~p~n", [B, R, F]),
	% {B} = Weightings,
	% io:format("Sample weights: ~p~n", [B]),
	% Modify state to new weightings
	NewState = State#state{weightings = Weightings},
	% Signal to Neurons to adjust weights to new values TODO ********************

	%%%% TOOO DOOOOO

	% io:format("NewState: ~p Incoming Weights: ~p~n", [NewState, Weightings]),

	{noreply, NewState};
%%% Pass inputs on to sensors
handle_info({sensor_inputs, {Inputs, ExpectedOutput}}, State) ->
	%% Cast this input off to the sensors TODO
	% io:format("NAME ~p CONFIG ~p GOT INPUT ~p SHOULD GIVE ~p~n", [State#state.name, State#state.weightings, Inputs, ExpectedOutput]),
	%% THIS SIMULATES WHAT THE ACTUATOR NEURONS WILL CALL
	{A, B} = Inputs,
	neuron:input_name(State#state.name, {s1, A}),
	neuron:input_name(State#state.name, {s2, B}),
	% Output = {A + 20, B + 20},
	% cortex:actuator_output(State#state.name, {Inputs, Output, ExpectedOutput}),
	NewState = State#state{inputs = Inputs, expectedOutput = ExpectedOutput},
	{noreply, NewState};
%%% Pass inputs on to sensors
handle_info({actuator_output, {ActuatorOutput}}, State) ->
	%% Handle output from actuators TODO
	%% Test if all actuators have reported back in

	%% If they have, shoot the result back to scape, and finish
	% StrippedName = list_to_atom(return_stripped_name(atom_to_list(State#state.name))),
	% io:format("Got actuator results, sending ~p back to ~p~n", [ActuatorOutput, State#state.name]),

	%%%%% TEMPORARY COMMENT


	scape:return_results(State#state.name, {State#state.inputs, ActuatorOutput, State#state.expectedOutput}),
	%{noreply, State};
	cortex:finish(State#state.name), %% Kill all neurons
	{stop, normal, State};

	%%%%% // TEMPORARY COMMENT

	% io:format("NAME: ~p Output: ~p Expected: ~p~n", [State#state.name, ActuatorOutput, State#state.expectedOutput]),

	% {noreply, State};
	%%%%%%%%

handle_info(_Message, _State) ->
	% io:format("Generic info handler: '~p' '~p'~n", [_Message, _State]),
	{noreply, _State}.
	%% Informative calls - possible return values
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
return_stripped_name(ScapeName) ->
    %%% Strip first 6 characters from the scape name
    PName = lists:sublist(ScapeName, 6),
    PName.

%%% Build a NN
build_net(State) ->
	%%%% 2 x 4 x 1
	% io:format(" BUILD NET NAME ~p~n", [State#state.name]),
	Name = State#state.name,
	% io:format("NAME ~p WEIGHTS ~p~n", [Name, State#state.weightings]),
	{{S1_w, S2_w}, {N1_w, N2_w, N3_w, _N4_w}, {A1_w}} = State#state.weightings,
	{{S1_b, S2_b}, {N1_b, N2_b, N3_b, _N4_b}, {A1_b}} = State#state.bias,

	%								   {name, {connections}}
	_N1 = neuron:new(Name, {s1, S1_w, S1_b, {n1, n2, n3}}),
	_N2 = neuron:new(Name, {s2, S2_w, S2_b, {n1, n2, n3}}),
	
	_N3 = neuron:new(Name, {n1, N1_w, N1_b, {a1}}),
	_N4 = neuron:new(Name, {n2, N2_w, N2_b, {a1}}),
	_N5 = neuron:new(Name, {n3, N3_w, N3_b, {a1}}),
	% N6 = neuron_sup:spawn_neuron(Name, {n4}),

	_N7 = neuron:new(Name, {a1, A1_w, A1_b, {}}),



	% io:format("Neurons: ~p ~p Name: ~p~n", [N1, N7, Name]),
	ok.