-module(irc_server_user_sup).
-behavior(supervisor).
-behavior(tcp_listener_accept_receiver).

%% API
-export([start_link/0, socket_accepted/2]).
-export([init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  RestartStrategy = {simple_one_for_one, 60, 3600},
  ChildSpec = {irc_server_user_server,
    {irc_server_user_server, start_link, []},
    temporary, 1000, worker, [irc_server_user_server]},

  {ok, {RestartStrategy, [ChildSpec]}}.

socket_accepted(AcceptedSocket, []) ->
  {ok, Pid} = supervisor:start_child(?MODULE, [AcceptedSocket]),
  irc_server_user_server:give_socket_control(AcceptedSocket, Pid),
  {ok, Pid}.
