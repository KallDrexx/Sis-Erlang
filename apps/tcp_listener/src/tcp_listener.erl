-module(tcp_listener).
-behavior(application).
-export([start/2, stop/1]).

%%% Tcp listener application

start(normal, _Args) -> tcp_listener_sup:start_link().
stop(_) -> ok.