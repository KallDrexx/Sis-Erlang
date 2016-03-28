-module(process_registry_server_tests).
-include_lib("eunit/include/eunit.hrl").
-define(setup(F), {setup, fun start/0, fun stop/1, F}).

can_register_and_retrieve_pid_via_key_test_() ->
  {"Registers self() and retrieves self() back with same key", ?setup(fun can_register_and_get_pid/1)}.

%% Setup functions
start() ->
  {ok, ServerPid} = process_registry_server:start_link(),
  ServerPid.

stop(ServerPid) -> gen_server:stop(ServerPid).

%% Tests
can_register_and_get_pid(ServerPid) ->
  ExpectedPid = self(),
  ok = process_registry_server:register_pid(ServerPid, ExpectedPid, "abc"),
  {ok, ReturnedPid} = process_registry_server:get_pid(ServerPid, "abc"),
  ?_assertEqual(ExpectedPid, ReturnedPid).
