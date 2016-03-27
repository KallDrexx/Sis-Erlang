-module(tcp_listener_server_tests).
-include_lib("eunit/include/eunit.hrl").
-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define(test_port, 9394).

-export([send_self_message/2]).

%% Test descriptions
server_calls_passed_in_mfa_test_() ->
  {"Server calls the passed in MFA", ?setup(fun test_mfa/1)}.

%% Setup functions
start() ->
  {ok, ListenSocket} = gen_tcp:listen(?test_port,[{active,false}, binary]),
  ListenSocket.

stop(ListenSocket) ->
  gen_tcp:close(ListenSocket).

%% Tests
test_mfa(ListenSocket) ->
  Mfa = {?MODULE, send_self_message, [self()]},

  {ok, Pid} = tcp_listener_server:start_link(ListenSocket, Mfa),
  ok = gen_server:cast(Pid, accept),
  {ok, _} = gen_tcp:connect({127,0,0,1}, ?test_port, []),

  receive
    success -> ?_assert(true);
    X -> ?_assert(X)
  after 1000 ->
    ?_assert(false)
  end.

%% Stubs
send_self_message(AcceptSocket, TestPid) ->
  case erlang:port_info(AcceptSocket) of
    undefined -> TestPid ! bad_accept_socket;
    _ -> TestPid ! success
  end.
