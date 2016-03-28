-module(process_registry_server_tests).
-include_lib("eunit/include/eunit.hrl").
-define(setup(F), {setup, fun start/0, fun stop/1, F}).

can_register_and_retrieve_pid_via_key_test_() ->
  {"Registers self() and retrieves self() back with same key",
    ?setup(fun can_register_and_get_pid/1)}.

can_overwrite_existing_pid_test_() ->
  {"Registers two pids with the same key, and retrieval gets the 2nd pid",
    ?setup(fun can_overwrite_existing_pid/1)}.

non_registered_pid_key_test_() ->
  {"Verifies retrieving a pid key that hasn't been registered gives undefined",
    ?setup(fun pidkey_not_registered_returns_undefined/1)}.

unregistered_pid_key_test_() ->
  {"Verfies pid keys can be unregistered",
    ?setup(fun unregistered_pidkey_returns_undefined/1)}.

%% Setup functions
start() ->
  {ok, ServerPid} = process_registry_server:start_link(),
  ServerPid.

stop(ServerPid) -> gen_server:stop(ServerPid).

%% Tests
can_register_and_get_pid(ServerPid) ->
  ExpectedPid = self(),
  ok = process_registry_server:register_pid(ServerPid, ExpectedPid, "abc"),
  ?_assertMatch({ok, ExpectedPid}, process_registry_server:get_pid(ServerPid, "abc")).

can_overwrite_existing_pid(ServerPid) ->
  SelfPid = self(),
  SecondPid = spawn(fun() -> ok end),

  ok = process_registry_server:register_pid(ServerPid, SelfPid, "abc"),
  ok = process_registry_server:register_pid(ServerPid, SecondPid, "abc"),
  ?_assertMatch({ok, SecondPid}, process_registry_server:get_pid(ServerPid, "abc")).

pidkey_not_registered_returns_undefined(ServerPid) ->
  ?_assertMatch(undefined, process_registry_server:get_pid(ServerPid, "abc")).

unregistered_pidkey_returns_undefined(ServerPid) ->
  SelfPid = self(),
  ok = process_registry_server:register_pid(ServerPid, SelfPid, "abc"),
  ok = process_registry_server:unregister_pid_key(ServerPid, "abc"),
  ?_assertMatch(undefined, process_registry_server:get_pid(ServerPid, "abc")).