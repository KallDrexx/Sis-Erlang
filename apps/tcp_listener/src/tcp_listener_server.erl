-module(tcp_listener_server).
-behavior(gen_server).
-record(state, {socket, on_accept_module, on_accept_arguments}).

%% API
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Socket, OnAcceptCallback) -> gen_server:start_link(?MODULE, [Socket, OnAcceptCallback], []).

init([Socket, {OnAcceptModule, OnAcceptArguments}]) ->
  gen_server:cast(self(), accept),
  State = #state{socket = Socket, on_accept_module = OnAcceptModule, on_accept_arguments = OnAcceptArguments},
  {ok, State}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(accept, State=#state{}) ->
  Module = State#state.on_accept_module,
  Arguments = State#state.on_accept_arguments,

  case gen_tcp:accept(State#state.socket) of
  {error, closed} -> {stop, normal, State};
  {ok, AcceptSocket} ->
    Module:socket_accepted(AcceptSocket, Arguments),

    gen_server:cast(accept, State),
    {noreply, State}
  end.

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.