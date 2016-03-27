-module(tcp_listener_server).
-behavior(gen_server).
-record(state, {socket, on_accept_mfa}).

%% API
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Socket, OnAcceptMfa) -> gen_server:start_link(?MODULE, [Socket, OnAcceptMfa], []).

init([Socket, OnAcceptMfa]) ->
  gen_server:cast(self(), accept),
  {ok, #state{socket = Socket, on_accept_mfa = OnAcceptMfa}}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(accept, State=#state{}) ->
  {Module, Function, Arguments} = State#state.on_accept_mfa,

  case gen_tcp:accept(State#state.socket) of
    {error, closed} -> {stop, normal, State};
    {ok, AcceptSocket} ->
      Pid = spawn(Module, Function, [AcceptSocket | Arguments]),
      ok = gen_tcp:controlling_process(AcceptSocket, Pid),

      gen_server:cast(accept, State),
      {noreply, State}
  end.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.