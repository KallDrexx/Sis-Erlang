-module(process_registry_sup).
-behavior(supervisor).

%% API
-export([init/1, start_link/0]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
  RestartStrategy = {one_for_one, 60, 3600},
  ChildSpec = {process_registry_server,
    {process_registry_server, start_link, []},
    permanent, 1000, worker, [process_registry_server]},

  {ok, {RestartStrategy, [ChildSpec]}}.