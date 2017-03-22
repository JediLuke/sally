%%====================================================================
%%
%% @author Luke Taylor
%%
%% @doc The lookup_table is responsible for deciding what lobes are correct in their reasoning
%%
%%====================================================================
-module(lookup_table).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% API
-export([start_link/1, stop/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% State variable
-record(state, {name}).


%%====================================================================
%% API functions
%%====================================================================
start_link(Params) ->
	%% io:format("lookup_table name: ~p~n",[Params]),
	%% Explicitely not assigning an ID to prevent 'already registered errors'
	%% as multiple lookup_tables will spawn this module.
	gen_server:start_link(?MODULE, [Params], []).

stop() ->
	%% Stop server asynchronously
	io:format("Stopping~n"),
	gen_server:cast(?MODULE, shutdown).


%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Params]) ->
	%% io:format("lookup_table: Initializing with ~p~n", [Params]),
	%% process_flag(trap_exit, true),
	NewState = #state{name=Params},
	{ok, NewState}.

%% Synchronous, possible return values
% {reply,Reply,NewState}
% {reply,Reply,NewState,Timeout}
% {reply,Reply,NewState,hibernate}
% {noreply,NewState}
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,Reply,NewState}
% {stop,Reason,NewState}
handle_call(Message, From, State) ->
	io:format("Generic call handler: '~p' from '~p' while in '~p'~n", [Message, From, State]),
	{reply, ok, State}.

%% Asynchronous, possible return values
% {noreply,NewState}
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
%% normal termination clause
handle_cast(shutdown, State) ->
	io:format("Generic cast handler: *shutdown* while in '~p'~n", [State]),
	{stop, by_request, State};
%% generic async handler
handle_cast(Message, State) ->
	io:format("Generic cast handler: '~p' while in '~p'~n", [Message, State]),
	{noreply, State}.

%% Informative calls
% {noreply,NewState}
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
% {stop,Reason,NewState}
handle_info({lobe, Inputs}, State) ->
	% Handle all information sent out to lobes (processing modules)
	% io:format("ARTILECT ~p LOBE lookup_table INPUTS: ~p~n", [State#state.name, Inputs]),
	% Think about these inputs TODO make Results a pre-defined record
	Results = process_input(Inputs),
	% Send these back to the orbito, who shall judge their worthiness
	ebus:pub(State#state.name, {lobe_update, lookup_table, Results}),
	% io:format("ARTILECT ~p LOBE lookup_table - done thinking :)~n", [State#state.name]),
	{noreply, State};
handle_info(_Message, _State) ->
	% io:format("Generic info handler: '~p' '~p'~n", [_Message, _State]),
	{noreply, _State}.

%% Server termination
terminate(_Reason, _State) ->
	io:format("Generic termination handler: '~p' '~p'~n", [_Reason, _State]).

%% Code change
code_change(_OldVersion, State, _Extra) ->
	{ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

process_input(Inputs) ->
	Inputs + 3.
	