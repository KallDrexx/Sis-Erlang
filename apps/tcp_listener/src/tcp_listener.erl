-module(tcp_listener).
-behavior(application).

-export([start/2, stop/1, start/4]).

start(normal, _Args) ->
  {ok, Port} = application:get_env(port),
  {ok, PoolSize} = application:get_env(acceptor_pool_size),
  {ok, OnAcceptModule} = application:get_env(on_accept_module),
  {ok, OnAcceptArguments} = application:get_env(on_accept_arguments),
  tcp_listener_sup:start_link({Port, PoolSize, OnAcceptModule, OnAcceptArguments}).

start(Port, PoolSize, OnAcceptModule, OnAcceptArguments) ->
  tcp_listener_sup:start_link({Port, PoolSize, OnAcceptModule, OnAcceptArguments}).

stop(_) -> ok.

