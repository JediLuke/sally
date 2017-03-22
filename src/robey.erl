-module(robey).
-compile(export_all).


%%==============================================================================
%%
%% @author Luke Taylor
%%
%% This module constructs objects, primarily used to represent state inside
%% gen_server processes.
%%
%% It is named after Dr. Mike Robey, Senior Lecturer at Curtin University,
%% who first taught me about object-oriented program design :)
%%
%%==============================================================================


% A RobeyGram is a non-mutable, object-like lamba function. Using Robey's gives
% the developer a key-store style storage mechanism, with several built-in functions.
%
% A Robey has the following properties:
%       - a type
%       - a list of tuples of the form (key, Value)
%
%   COMING SOON
%
%   Methods, when they are called, produce the next-generation of the robey according to the method & specs called in the next cons


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constructor
cons(Type) ->
    cons(Type, [], 0).


%%% Construct from a saved list of StateVariables
cons(Type, StateVariables) ->
    cons(Type, StateVariables, 0).


%%% With in-built methods
cons(Type, StateVariables, MethodList) -> fun(Action, Data) ->

    %%% Procedures
    case Action of

    	%%% Identifier - responds with 'object' type
    	identify ->
    		{type, Type};

    	%%% Getter - get value of a state variable
        get ->
            case lists:keyfind(Data, 1, StateVariables) of
                {Data, Result} -> Result
            end;
        getStateList ->
            StateVariables;

        %%% Add new variable tuple to the StateVariables list
        add ->
            {_Key, _NewValue} = Data, % Formatting check
            NewStateVars = StateVariables ++ [Data],
            cons(Type, NewStateVars);

        %%% Setter - update a state variable
        set ->
            {Key, _NewValue} = Data,
            case lists:keyfind(Key, 1, StateVariables) of
                {Key, _OldValue} ->
                    % Remove the old variable entry from the list of StateVariables
                    ReducedVars = lists:delete({Key, _OldValue}, StateVariables),
                    % Re-add the variable in with the new value
                    FullVars = ReducedVars ++ [Data],
                    % Construct a new object using the new FullVars list
                    cons(Type, FullVars)
            end;

        %%% Save - save object state to a file
        save ->
            % Clear out old file first as save_to_file appends text if a file already exists
            file:delete(Data), %% The filename is passed in as the Data variable
            save_to_file(Data, StateVariables);

        %%% toString - print the current state to the terminal
        num_variables ->
            length(StateVariables);

        %%% isEqual - Check if two 'classes' have the same 'class fields' and type
        isEqual ->
            % Check types and state variables are equal. We pass in '0' for both
            % calls as data is a _DontCare variable, but we must pass something.
        	{type, Type} = Data(identify, 0), 
            (StateVariables == Data(getStateList, 0)); %%TODO Bug: If lists are the same, but in different order, this test should pass but in fact fails

        %%% toString - print the current state to the terminal
        toString ->
            io:format("RobeyState: ~p~n", [StateVariables])

    	end % END procedures case


	end. % END cons


%%% Construct new state from save file
load(Type) ->
    load(Type, "object.txt"). %% Default file
load(Type, DataFile) ->
    {ok, LoadedState} = file:consult(DataFile),
    cons(Type, LoadedState).


%%%=============================================================================
%%% Internal functions
%%%=============================================================================


%%% Save list of tuples to a file
save_to_file(_DataFile, []) -> ok;
save_to_file(DataFile, [H|Tail]) ->
    file:write_file(DataFile, io_lib:format("~p.~n", [H]), [append]),
    save_to_file(DataFile, Tail).