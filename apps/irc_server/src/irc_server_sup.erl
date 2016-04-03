-module(irc_server_sup).
-behavior(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  RestartStrategy = {one_for_one, 60, 3600},

  UserSupervisorChildSpec = {irc_server_user_sup,
    {irc_server_user_sup, start_link, []},
    permanent, 1000, supervisor, [irc_server_user_sup]},

  TcpListenerChildSpec = {tcp_listener,
    {tcp_listener, start_link, [6667, 20, irc_server_user_sup, []]},
    permanent, 1000, worker, [tcp_listener]},

  {ok, {RestartStrategy, [UserSupervisorChildSpec, TcpListenerChildSpec]}}.