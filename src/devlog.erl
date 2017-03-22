-module(devlog).
-compile(export_all).


%%==============================================================================
%%
%% @author Luke Taylor
%%
%% Libary for logging things in Erlang.
%%
%%
%%==============================================================================


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
debug(Msg) ->
    io:format("DevLog: ~p~n", [Msg]).

debug(Msg1, Msg2) ->
    io:format("DevLog: ~p ~p~n", [Msg1, Msg2]).