-module(tcp_listener_server).
-behavior(gen_server).
-record(state, {socket}).

%% API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Socket) -> gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
  gen_server:cast(self(), accept),
  {ok, #state{socket = Socket}}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(accept, State=#state{socket = Socket}) ->
  {ok, AcceptSocket} = gen_tcp:accept(Socket),
  tcp_listener_sup:start_acceptor(),
  {noreply, State#state{socket = AcceptSocket}}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.