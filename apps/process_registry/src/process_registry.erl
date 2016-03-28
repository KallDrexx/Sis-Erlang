-module(process_registry).
-behavior(application).

%% API
-export([start/2, stop/1, register/2, unregister/1, get_pid/1]).

start(normal, _) -> process_registry_sup:start_link().
stop(_) -> ok.

register(Pid, PidKey) -> process_registry_server:register_pid(Pid, PidKey).
unregister(PidKey) -> process_registry_server:unregister_pid_key(PidKey).
get_pid(PidKey) -> process_registry_server:get_pid(PidKey).