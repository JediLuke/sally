-module(neuron).
-behaviour(gen_server).
-define(SERVER, ?MODULE).


-compile(export_all).


%%====================================================================
%%
%% @author Luke Taylor
%%
%%====================================================================


%
%	neuron.erl
%
%	This is the model for a neuron
%
%
%	==(n)==> O(w, b) -> sigma(f(O.w,n) + O.b)
%
%
%	An input vector of size n contains signals. These signals are processed
%	using internal variables w (weights) and b (bias). The output is then
%	sent out as another signal
%


-export([cons/1, 
		input/2		%% Attach an input to this neuron
		]). 
-export([init/1, handle_call/3, handle_cast/2,
		 handle_info/2, terminate/2, code_change/3]).


%%====================================================================
%% API functions
%%====================================================================


%% DEVLOG: This whole section needs to be cleaned up

-export([start_link/2, stop/0]).
-export([new/2,						%% Spawn a new neuron
		 input_name/2				%% Send a neuron input by the name of the net TODO DEPRECATE
		 %input/2					%% Assign new signal to input channel
		 %set_bias/1,				%% Assign a new value for the bias
		 %set_weighting/2			%% Assign a new value for one of the weightings
		 %perturb/1					%% Command to perturb the weightings and bias by some magnitude 
		 ]).	


% start_link(NN_Name, NeuronId, WeightList, Bias, ConnectionList) ->
start_link(NN_Name, NeuronGene) ->
	gen_server:start_link(?MODULE, [NN_Name, NeuronGene], []).


stop() ->
	%% Stop server asynchronously
	io:format("Stopping~n"),
	gen_server:cast(?MODULE, shutdown).


input_name(NN_Name, {NeuronName, Value}) ->
	%% Stop server asynchronously
	ebus:pub(NN_Name, {NeuronName, Value}).

new(NN_Name, {NeuronID, WeightList, Bias, Connectionlist}) ->
	PiD = neuron_sup:spawn_neuron(NN_Name, {NeuronID, WeightList, Bias, Connectionlist}),
	%NeuronFunc = fun(PiD) ->
	PiD.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API functions


%%% Construct a new NeuronRef object
cons(_NeuronGene) ->
	% {NetName, NeuronID, WeightList, Bias, Connectionlist} = {test_net, s2, [0.2, 0.3, 0.4], {bias, 1}, [{n1, n2, n3}]},

	NN_Name = test_net, %% TODO This needs to be passed in either as an argument or as part of NeuronGene

	NeuronGene = robey:cons(neuron_gene, [
		{neuron_id, s1},
		{weight_list, [
			{1, 0.1},
			{2, 0.2},
			{3, 0.3}]},
		{bias, 1},
		{cnx_list, {n1, n2, n3}}]),
		% Activation func should be part of NeuronGene??


		% {weights, robey:cons(vector, [
		% 	{bandwidth,  3},
		% 	{weight_list, [{1, 0.1}, {2, 0.2}, {3, 0.3}]}])},
		% {config, robey:cons(config, [
		% 	{weights, robey:cons(vector, [
		% 		{bandwidth,  3},
		% 		{weight_list, [{1, 0.1}, {2, 0.2}, {3, 0.3}]}])},
		% 	{bias,	  Bias},
		% 	{connex,  Connectionlist}])}
		% ]),


	%{NetName, NeuronID, WeightList, Bias, Connectionlist} = NeuronGene,
	PiD = neuron_sup:spawn_neuron(NN_Name, NeuronGene),
	robey:cons(neuron_ref, [{pid, PiD}]).

input(NeuronRef, {Channel, Signal}) ->
	gen_server:cast(NeuronRef(get, pid), {Channel, Signal}).

%%% attach - Called by another neuron, to attach to this neuron
% attach(NeuronRef) ->

%%% reach_out - Gets the current neuron to extend out a 'feeler' and attach to another neuron/s

%%% extract - Withdraw answer by querying input nodes

print(NeuronRef) ->
	gen_server:cast(NeuronRef(get, pid), print).

%%%=============================================================================
%% gen_server callbacks
%%%=============================================================================


% init([NN_Name, NeuronID, Weights, Bias, ConnectionList]) -
init(InputList) ->
	[NN_Name, NeuronGene] = InputList,
	%% Call constructor

	% NeuronVariables = [
 %    	{nn_name,    NN_Name},		
 %    	{neuron_id,  NeuronID},
 %    	{weightlist, WeightList},			%% TODO GOTTA GO
 %    	{bias, 		 Bias},					%% TODO GOTTA GO
 %    	{cx_list, 	 ConnectionList},
 %    	{inputlist,	 []}
 %    ],

	NeuronState = robey:cons(neuron, [
		{net_name,  NN_Name},
		{neuron_id, NeuronGene(get, neuron_id)},
		{inputs, robey:cons(inputs, [
			{bandwidth, length((NeuronGene(get, weight_list)))},
			{ch_list,	[]}])}, %% Initialize input channels as empty
		{config, robey:cons(config, [
			{weights, NeuronGene(get, weight_list)},
			{bias,	  NeuronGene(get, bias)},
			{connex,  NeuronGene(get, cnx_list)}])},
		{func, sigmoid_lambda()}
		]),

	{ok, NeuronState}.


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
%% Handle signals being but through to channel
handle_cast({Channel, Signal}, NeuronState) ->
	devlog:debug({Channel, Signal}),
	NewNeuronState = process_input(NeuronState, {Channel, Signal}),
	{noreply, NewNeuronState};
%%% normal termination clause
handle_cast(print, NeuronState) ->
	NeuronState(toString, 0),
	devlog:debug("INPUTS:", (NeuronState(get, inputs))),
	(NeuronState(get, config))(toString, 0),
	{noreply, NeuronState};
%%% normal termination clause
handle_cast(shutdown, State) ->
	io:format("Generic cast handler: *shutdown* while in '~p'~n", [State]),
	{stop, by_request, State};
%% generic async handler
handle_cast(Message, State) ->
	io:format("Generic cast handler: '~p' while in '~p'~n", [Message, State]),
	{noreply, State}.
	%% Asynchronous, possible return values
	%% {noreply, NewState} | {noreply, NewState, Timeout} | {noreply,NewState,hibernate} | {stop,Reason,NewState}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% handle_info
handle_info({nn, soundoff}, State) ->
	io:format("NEURON MODULE: Name: ~p~n", [State(get, nn_name)]),
	{noreply, State};
handle_info({neuron, Inputs}, State) ->
	io:format("neuron INPUT: ~p~n", [Inputs]),
	{noreply, State};

%%% Net Message passing
handle_info({s1, InputValue}, State) ->
	
	% neuron:input_name(State(get, nn_name), {n3, Inputs}),
	%%% Perform computation on Neuron
	IsSame = same_neuron(s1, State(get, neuron_id)),
	case IsSame of
		true ->
			% io:format("S1 neuron INPUT: ~p~n", [InputValue]),
			NewInputList = State(get, inputlist) ++ [InputValue],
			Output = activation_func(NewInputList, State(get, weightlist), State(get, bias)),
			neuron:input_name(State(get, nn_name), {n1, [Output]}), %% TODO use connectList
			neuron:input_name(State(get, nn_name), {n2, [Output]}), %% TODO use connectList
			neuron:input_name(State(get, nn_name), {n3, [Output]}); %% TODO use connectList
		_Else ->
			pass
	end,



	{noreply, State};
handle_info({s2, InputValue}, State) ->
	
	%%% Perform computation on Neuron
	IsSame = same_neuron(s2, State(get, neuron_id)),
	case IsSame of
		true ->
			% io:format("S2 neuron INPUT: ~p~n", [InputValue]),
			NewInputList = State(get, inputlist) ++ [InputValue],
			Output = activation_func(NewInputList, State(get, weightlist), State(get, bias)),
			% io:format("Name: ~p S2-output ~p~n", [State(get, nn_name), Output]),
			neuron:input_name(State(get, nn_name), {n1, [Output]}), %% TODO use connectList
			neuron:input_name(State(get, nn_name), {n2, [Output]}), %% TODO use connectList
			neuron:input_name(State(get, nn_name), {n3, [Output]}); %% TODO use connectList
		_Else ->
			pass
	end,
	{noreply, State};
handle_info({n1, Inputs}, State) ->
	
	IsSame = same_neuron(n1, State(get, neuron_id)),
	case IsSame of
		true ->
			% io:format("N1 neuron INPUT: ~p~n", [Inputs]),
			% %% Add results to results cache
			NewInputList = lists:append(State(get, inputlist), Inputs),
			%% SOURCE OF BIG ERROR: Need some way to track when all different cortexes have finished that's not just counting cycles
			% io:format("Inputs: ~p NewInputList: ~p~n", [Inputs, NewInputList]),
			L = length(NewInputList),
			case L of
				2 ->
					NewState = State(set, {inputlist, NewInputList}),
					Output = activation_func(NewInputList, State(get, weightlist), State(get, bias)),
					% io:format("Name: ~p N1-output ~p~n", [State(get, nn_name), Output]),
					neuron:input_name(State(get, nn_name), {a1, [Output]});
				_Else ->
					NewState = State(set, {inputlist, NewInputList})
			end;
		_Else ->
			NewState = State
	end,
	{noreply, NewState};
handle_info({n2, Inputs}, State) ->
	
	IsSame = same_neuron(n2, State(get, neuron_id)),
	case IsSame of
		true ->
			% io:format("N2 neuron INPUT: ~p~n", [Inputs]),
			% %% Add results to results cache
			NewInputList = lists:append(State(get, inputlist), Inputs),
			%% SOURCE OF BIG ERROR: Need some way to track when all different cortexes have finished that's not just counting cycles
			L = length(NewInputList),
			case L of
				2 ->
					NewState = State(set, {inputlist, NewInputList}),
					Output = activation_func(NewInputList, State(get, weightlist), State(get, bias)),
					% io:format("Name: ~p HHHH~n", [State(get, nn_name)]),
					neuron:input_name(State(get, nn_name), {a1, [Output]});
				_Else ->
					NewState = State(set, {inputlist, NewInputList})
			end;
		_Else ->
			NewState = State
	end,
	{noreply, NewState};
handle_info({n3, Inputs}, State) ->
	
	IsSame = same_neuron(n3, State(get, neuron_id)),
	case IsSame of
		true ->
			% io:format("N3 neuron INPUT: ~p~n", [Inputs]),
			% %% Add results to results cache
			NewInputList = lists:append(State(get, inputlist), Inputs),
			%% SOURCE OF BIG ERROR: Need some way to track when all different cortexes have finished that's not just counting cycles
			L = length(NewInputList),
			case L of
				2 ->
					NewState = State(set, {inputlist, NewInputList}),
					Output = activation_func(NewInputList, State(get, weightlist), State(get, bias)),
					% io:format("Name: ~p HHHH~n", [State(get, nn_name)]),
					neuron:input_name(State(get, nn_name), {a1, [Output]});
				_Else ->
					NewState = State(set, {inputlist, NewInputList})
			end;
		_Else ->
			NewState = State
	end,
	{noreply, NewState};
handle_info({a1, Inputs}, State) ->
	
	%%% Perform computation on Neuron
	IsSame = same_neuron(a1, State(get, neuron_id)),
	case IsSame of
		true ->
			% io:format("ACTUATOR INPUT: ~p~n", [Inputs]),
			% %% Add results to results cache
			NewInputList = lists:append(State(get, inputlist), Inputs),
			%% SOURCE OF BIG ERROR: Need some way to track when all different cortexes have finished that's not just counting cycles
			L = length(NewInputList),
			case L of
				3 ->
					NewState = State(set, {inputlist, NewInputList}),
					Output = activation_func(NewInputList, State(get, weightlist), State(get, bias)),
					%io:format("Name: ~p Output: ~p~n", [State(get, nn_name), Output]),
					% neuron:input_name(State(get, nn_name), {a1, Output});
					cortex:actuator_output(State(get, nn_name), {Output});
				_Else ->
					NewState = State(set, {inputlist, NewInputList})
			end;
		_Else ->
			NewState = State
	end,
	{noreply, NewState};


%%% Finish the sim
handle_info({finish}, State) ->
	{stop, normal, State};
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Neuron operation functions
%%% Process it when an input comes in
process_input(NeuronState, {Channel, Signal}) ->
	%% TODO Idea: Always firing mode - don't wait for all inputs, re-calculate activation function and send output every time we get input
	%% TODO Don't just blindly add to list, check to see if this channel is taken already
	InputList = (NeuronState(get, inputs))(get, ch_list) ++ [{Channel, Signal}],
	case length(InputList) == (NeuronState(get, inputs))(get, bandwidth) of
		true ->
			fire_synapse(NeuronState, InputList),
			% Reset NeuronState to have no inputs
			NewNeuronState = NeuronState(set, {inputs, robey:cons(inputs, [
									{bandwidth, (NeuronState(get, inputs))(get, bandwidth)},
									{ch_list,	[]}])});
		_Otherwise ->
			NewNeuronState = NeuronState(set, {inputs, robey:cons(inputs, [
									{bandwidth, (NeuronState(get, inputs))(get, bandwidth)},
									{ch_list,	InputList}])})
	end,
	NewNeuronState.


%%% Calculate output and send it through to all recipient neurons
fire_synapse(NeuronState, InputList) ->
	Sigma = NeuronState(get, func),
	WeightsList = (NeuronState(get, config))(get, weights),
	Output = Sigma(dot_product(WeightsList, InputList) + (NeuronState(get, config))(get, bias)),
	devlog:debug("FIRING", Output).


%%% Calculate dot product of two one-dimensional lists
dot_product(A, B) ->
	% devlog:debug("A:", A),
	% devlog:debug("B:", B),
	dot_product(A, B, 0).
%% TODO Check Channels are aligned with eachother - assume for now it's ok, we seem to be gettinng consistent results
dot_product([{ChA, ValA}|As], [{ChB, ValB}|Bs], 0)	 -> dot_product(As, Bs, (ValA*ValB));
dot_product([{ChA, ValA}|As], [{ChB, ValB}|Bs], Acc) -> dot_product(As, Bs, (ValA*ValB) + Acc);
dot_product([], [], Acc) -> Acc.


%%% Lambdas
sigmoid_lambda() -> fun(T) ->
	(1 / (1 + math:exp(T)))
	end.

hypertan_lambda() -> fun(X) ->
	math:tanh(X)
	end.


%%%=============================================================================
%%% Internal functions - DEPRECATE LATER
%%%=============================================================================


activation_func(InputList, WeightList, Bias) ->
	% io:format("InputList: ~p WeightList ~p~n", [InputList, WeightList]),
	% io:format("BIASAAA~p~n" , [Bias]),
	[B] = Bias,
	% Raw_Out = dot_product(InputList, WeightList, 0) + B,
	Raw_Out = dot_product(InputList, WeightList, 0), %% TODO WHEN BIAS IS WORKING PROPERLY USE THE OTHER ONE
	% Output = math:tanh(Raw_Out),
	Output = Raw_Out,
	%io:format("ACT_FUNC Output: ~p~n", [Output]),
	T = trunc(Output, 4),
	% if
	%     T > 1 ->
	%         %io:format("--- Update:: count: ~p gBest-Error ~p bestConfig ~p~n", [NewCount, State#state.gBest, State#state.bestConfig]);
	%         Out2 = 1;
	%     T < 0 ->
	%         Out2 = 0;
	%     true ->
	%         Out2 = T
	% end,
	% Out2.
	T.




same_neuron(MsgRecipient, SensorId) ->
	 case MsgRecipient == SensorId of
		true ->
			true;
		_Else ->
			false
	end.

%%% Truncates a floating point number F to N decimal places ignoring trailing zeros.
%%% http://www.qlambda.com/2013/10/erlang-truncate-floating-point-number.html
-spec trunc(float()|integer(), integer()) -> float().
trunc(Num, NumDigits) ->
	Prec = math:pow(10, NumDigits),
	trunc(Num*Prec)/Prec.