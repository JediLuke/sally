%%====================================================================
%%
%% @author Luke Taylor
%% 
%% This module is a new behaviour things will implement, which shall
%% handle things like spawning children, the Gen_server BS, memory and pub/sub
%% All through a nice API
%%
%%====================================================================
-module(my_node).
-behaviour(node).
-define(SERVER, ?MODULE).

%% API
-export([test/1, new_node/0]).

% -record(state, {name}).


%%====================================================================
%% API functions
%%====================================================================

test(fakeIn) ->
    {ok, myResult}.

new_node() ->
    {ok, _PiD} = node:start_link(1),
    ok.