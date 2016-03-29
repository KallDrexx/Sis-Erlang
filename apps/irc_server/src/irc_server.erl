-module(irc_server).
-behavior(application).

-export([start/2, stop/1]).

start(StartType, StartArgs) ->
  erlang:error(not_implemented_na).

stop(State) ->
  erlang:error(not_implemented).