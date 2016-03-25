-module(tcp_listener_sup).
-behavior(supervisor).

%% API
-export([start_link/0]).
-export([init/1, start_acceptor/0]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

  {ok, Port} = application:get_env(port),
  {ok, PoolSize} = application:get_env(acceptor_pool_size),
  {ok, ListenSocket} = gen_tcp:listen(Port, [{active, once}, {packet, line}]),
  spawn_link(fun() -> start_initial_listeners(PoolSize) end),

  RestartStrategy = {simple_one_for_one, 60, 3600},
  ChildSpec = {tcp_listener_server, {tcp_listener_server, start_link, [ListenSocket]},
    temporary, 1000, worker, [tcp_listener_server]},

  {ok, {RestartStrategy, [ChildSpec]}}.

start_acceptor() -> supervisor:start_child(?MODULE, []).

%% Internal
start_initial_listeners(PoolSize) when is_number(PoolSize) ->
  [start_acceptor() || _ <- lists:seq(1, PoolSize)],
  ok.


