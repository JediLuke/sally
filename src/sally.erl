%%%-------------------------------------------------------------------
%%% @doc sally public API
%%% @end
%%%-------------------------------------------------------------------
-module(sally).
-behaviour(application).


%%% External interface functions
-export([input/2,			%% Send an artilect some new input
		 spawn_artilect/0,	%% Spawn a new artilect
		 train/1,			%% Starts training cycle of an artilect
		 go/0,				%% Manual kickstart of pre-configured test scenario
		 ctx_t/0			%% TEMPORARY test thingy
		 ]).
-export([start/2, stop/1]).	%% Required for an Erlang application



%%====================================================================
%% API
%%====================================================================


%% Sends a message (Input) to the artilect specified by (Name)
input(Artilect, Input) ->
	artilect:input(Artilect, Input).


%% Spawns a new Artilect
spawn_artilect() ->
	artilect:new().


%% Starts the training cycle of an artilect
train(Artilect) ->
	artilect:train(Artilect).


% Create new artilect and start it up
go() ->
	artilect:train(artilect:new()).


% Create new artilect and start it up
ctx_t() ->
	IdealConfig = {{[1.0], [1.0]}, {[1.0, 0.0], [0.0, 0.0], [0.0, 1.0], [0.0, 0.0]}, {[1.0, 0.0, 1.0]}},
	cortex_sup:spawn_cortex(lauren, IdealConfig),
    % cortex:construct_net(State#state.name, InputConfig),
    %% Send inputs to Cortex's
    Input = {1, 1}, ExpectedOutput = 1,
    cortex:sensor_input(lauren, {Input, ExpectedOutput}).


%%====================================================================
%% Application callbacks and functions
%%====================================================================


start(_StartType, _StartArgs) ->
	% Setup
	display_welcome_msg(),
	configure_cowboy(),
	application:start(ebus),	% ebus is a pub/sub messaging bus we use
	% Begin application
	sally_sup_top:start_link().	% Start supervision tree. Annoyingly, application:start() seems to need to return {ok, PiD}


stop(_State) ->
	ok.


%%====================================================================
%% Internal functions
%%====================================================================


% Configure cowboy webserver
configure_cowboy() ->
	{ok, _} = application:ensure_all_started(cowboy),
	Dispatch = cowboy_router:compile([
        {'_', [
        	{"/", cowboy_static, {priv_file, sally, "static/index.html"}},
        	{"/toppage", toppage_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, 100, [{port, 8080}], #{
    	env => #{dispatch => Dispatch}
    }),
    ok.


% Prints the welcome message when the application starts
display_welcome_msg() ->
	io:format("~n--- Sally --- ~n"),
	io:format("~n If you build an Artilect, at least name it after a beautiful soul :) ~n"),
	io:format("~n--- Artilect training platform ---~n"),
	io:format("~nInterfaces-~n"),
	io:format("----------------------~n"),
	io:format("input/2			(Artilect, Input)~n"),
	io:format("spawn_artilect/3	(Name, FitnessFunc, Options)~n"),
	io:format("~n=== Starting ===~n~n").