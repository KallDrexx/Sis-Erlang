-module(tcp_listener_sup).
-behavior(supervisor).

%% API
-export([start_link/1]).
-export([init/1]).

start_link({Port, PoolSize, OnAcceptModule, OnAcceptArguments}) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [{Port, PoolSize, OnAcceptModule, OnAcceptArguments}]).

init([{Port, PoolSize, OnAcceptModule, OnAcceptArguments}]) ->
  {ok, ListenSocket} = gen_tcp:listen(Port, [{active, false}]),
  spawn_link(fun() -> start_initial_listeners(PoolSize) end),

  RestartStrategy = {simple_one_for_one, 60, 3600},
  ChildSpec = {tcp_listener_server,
    {tcp_listener_server, start_link, [ListenSocket, {OnAcceptModule, OnAcceptArguments}]},
    permanent, 1000, worker, [tcp_listener_server]},

  {ok, {RestartStrategy, [ChildSpec]}}.

%% Internal
start_initial_listeners(PoolSize) when is_number(PoolSize) ->
  [supervisor:start_child(?MODULE, []) || _ <- lists:seq(1, PoolSize)],
  ok.


