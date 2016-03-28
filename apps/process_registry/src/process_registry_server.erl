-module(process_registry_server).
-behavior(gen_server).
-record(state, {known_pids}).

%% API
-export([start_link/0, register_pid/3, unregister_pid_key/2, get_pid/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() -> gen_server:start_link(?MODULE, [], []).

register_pid(RegistryServerRef, PidToRegister, PidKey) ->
  gen_server:cast(RegistryServerRef, {register, {PidToRegister, PidKey}}).

unregister_pid_key(RegistryServerRef, PidKey) ->
  gen_server:cast(RegistryServerRef, {unregister, PidKey}).

get_pid(RegistryServerRef, PidKey) ->
  gen_server:call(RegistryServerRef, {get_pid, PidKey}).

init(_Args) ->
  {ok, #state{known_pids = orddict:new()}}.

handle_call({get_pid, PidKey}, _From, State=#state{}) ->
  case orddict:find(PidKey, State#state.known_pids) of
    {ok, Pid} -> {reply, {ok, Pid}, State};
    error -> {reply, undefined, State}
  end.

handle_cast({register, {PidToRegister, PidKey}}, State=#state{known_pids = KnownPids}) ->
  NewState = State#state{known_pids = orddict:store(PidKey, PidToRegister, KnownPids)},
  {noreply, NewState};

handle_cast({unregister, PidKey}, State=#state{known_pids = KnownPids}) ->
  NewState = State#state{known_pids = orddict:erase(PidKey, KnownPids)},
  {noreply, NewState}.

handle_info(_Info, _State) ->
  erlang:error(not_implemented).

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) ->  {ok, State}.