-module(irc_server).
-behavior(application).

-export([start/2, stop/1]).

start(normal, _StartArgs) -> irc_server_sup:start_link().
stop(_State) -> ok.