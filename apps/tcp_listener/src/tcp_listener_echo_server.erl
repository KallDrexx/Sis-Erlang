-module(tcp_listener_echo_server).
-behavior(gen_server).
-behavior(tcp_listener_accept_receiver).
-record(state, {socket}).
-define(TcpMessage(Message), {tcp, _Port, Message}).

%% API
-export([start/1, socket_accepted/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

socket_accepted(AcceptedSocket, _Arguments) ->
  start(AcceptedSocket).

start(Socket) ->
  io:format("Start called ~n", []),
  {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, Socket, []),
  ok = gen_tcp:controlling_process(Socket, Pid),
  Pid ! ready,
  {ok, Pid}.

init(Socket) ->
  io:format("init called ~n", []),
  send(Socket, "Welcome!", []),
  {ok, #state{socket = Socket}}.

handle_call(Request, _From, State) ->
  io:format("Unhandled call received: ~p~n", [Request]),
  {noreply, State}.

handle_cast(Request, State) ->
  io:format("Unhandled cast received: ~p~n", [Request]),
  {noreply, State}.

handle_info(ready, State) ->
  ok = inet:setopts(State#state.socket, [list, {active, once}, {packet, line}]),
  {noreply, State};

handle_info(?TcpMessage("quit"++_), State) ->
  send(State#state.socket, "goodbye!", []),
  gen_tcp:close(State#state.socket),
  {stop, normal, State};

handle_info(?TcpMessage(Message), State) ->
  io:format("Message received: ~s", [Message]),
  send(State#state.socket, ">> ~s", [Message]),
  {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
  io:format("Socket closed~n", []),
  {stop, normal, State};

handle_info({tcp_error, _Socket, Reason}, State) ->
  io:format("Socket error: ~p~n", [Reason]),
  {stop, normal, State};

handle_info(Info, State) ->
  io:format("Unhandled message received: ~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("Terminating~n", []),
  ok.

code_change(_OldVsn, State, _Extra) ->
  io:format("Code changed~n", []),
  {ok, State}.

%% Private functions
send(Socket, Str, Args) ->
  ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
  ok = inet:setopts(Socket, [{active, once}]),
  ok.
