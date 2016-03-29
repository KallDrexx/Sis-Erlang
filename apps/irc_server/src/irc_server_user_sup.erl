-module(irc_server_user_sup).
-behavior(supervisor).

%% API
-export([init/1]).

init(Args) ->
  erlang:error(not_implemented).