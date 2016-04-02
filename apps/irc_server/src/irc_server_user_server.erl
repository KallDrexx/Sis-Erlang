-module(irc_server_user_server).
-behavior(gen_server).
-include_lib("irc_server/include/irc_commands.hrl").

-record(state, {socket, nick=""}).
-define(TcpMessage(Message), {tcp, _Port, Message}).

%% API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(AcceptedSocket) ->
  {ok, Pid} = gen_server:start_link(?MODULE, AcceptedSocket, []),
  gen_tcp:controlling_process(AcceptedSocket, Pid),
  Pid ! socket_ready,
  {ok, Pid}.

init(AcceptedSocket) ->
  {ok, #state{socket = AcceptedSocket}}.

handle_call(Request, _From, State) ->
  io:format("Unhandled call received: ~p~n", [Request]),
  {noreply, State}.

handle_cast(Request, State) ->
  io:format("Unhandled cast received: ~p~n", [Request]),
  {noreply, State}.

%% Prevents possible race condition that init/1 is called prior
%% to the server becoming the controlling process
handle_info(socket_ready, State=#state{socket = Socket}) ->
  ok = inet:setopts(Socket, [{active, once}, {packet, line}]),
  {noreply, State};

handle_info(?TcpMessage(Message), State) ->
  ParsedCommand = irc_command:parse(Message),
  {ok, NewState} = handle_command(ParsedCommand, State),
  ok = inet:setopts(State#state.socket, [{active, once}]),
  {noreply, NewState};

handle_info({tcp_closed, _Socket}, State) ->
  io:format("Socket closed~n", []),
  {stop, normal, State};

handle_info({tcp_error, _Socket, Reason}, State) ->
  io:format("Socket error: ~p~n", [Reason]),
  {stop, normal, State};

handle_info(Info, State) ->
  io:format("Unhandled info received: ~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Private functions
send(Socket, Str, Args) ->
  ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
  ok = inet:setopts(Socket, [{active, once}]),
  ok.

handle_command(#nick_command{nick_name = NickName}, State) ->
  io:format("Nickname set to ~s~n", [NickName]),
  {ok, State#state{nick = NickName}};

handle_command(#user_command{}, State) ->
  io:format("User command received~n", []),
  {ok, State};

handle_command(Command, State) ->
  io:format("Unknown command received: ~p~n", [Command]),
  {ok, State}.