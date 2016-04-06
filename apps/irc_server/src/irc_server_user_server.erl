-module(irc_server_user_server).
-behavior(gen_server).
-include_lib("irc_server/include/irc_received_commands.hrl").
-include_lib("irc_server/include/irc_server_messages.hrl").
-include_lib("irc_server/include/irc_channel.hrl").

-define(TcpMessage(Message), {tcp, _Port, Message}).
-define(ProcessGroup(NickName), "irc_user_" ++ NickName).

-record(state, {socket, nick="", username="", logged_in=false, host_address, mask=""}).
-record(user_message, {raw_message=""}).

%% API
-export([start_link/1, socket_accepted/2, give_socket_control/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(AcceptedSocket) ->
  {ok, Pid} = gen_server:start_link(?MODULE, AcceptedSocket, []),
  give_socket_control(AcceptedSocket, Pid),
  {ok, Pid}.

socket_accepted(AcceptedSocket, []) -> start_link(AcceptedSocket).

give_socket_control(Socket, Pid) ->
  case gen_tcp:controlling_process(Socket, Pid) of
    ok -> gen_server:cast(Pid, socket_ready);
    {error, not_owner} -> ok % not owner so assume owner will take care of this
  end.

init(AcceptedSocket) ->
  {ok, {Address, _Port}} = inet:peername(AcceptedSocket),
  {ok, #state{socket = AcceptedSocket, host_address = Address}}.

handle_call(Request, _From, State) ->
  io:format("irc_server_user_server: Unknown call: ~p~n", [Request]),
  {noreply, State}.

handle_cast(socket_ready, State=#state{socket = Socket}) ->
  %% Prevents possible race condition of trying to call int:setopts prior
  %% to the server becoming the controlling process
  io:format("Received socket ready message~n", []),
  ok = inet:setopts(Socket, [list, {active, once}, {packet, line}]),
  {noreply, State};

handle_cast(Request, State) ->
  io:format("irc_server_user_server: Unknown cast: ~p~n", [Request]),
  {noreply, State}.

handle_info(?TcpMessage(Message), State) ->
  io:format("~p received~n", [Message]),

  ParsedCommand = irc_received_commands:parse(Message),
  {ok, NewState} = handle_command(ParsedCommand, State),
  ok = inet:setopts(State#state.socket, [{active, once}]),
  {noreply, NewState};

handle_info({tcp_closed, _Socket}, State) ->
  io:format("Socket closed~n", []),
  unregister_user_process(State#state.nick),
  {stop, normal, State};

handle_info({tcp_error, _Socket, Reason}, State) ->
  io:format("Socket error: ~p~n", [Reason]),
  unregister_user_process(State#state.nick),
  {stop, normal, State};

handle_info(#user_message{raw_message = Message}, State) ->
  send(State#state.socket, Message),
  {noreply, State};

handle_info(Info, State) ->
  io:format("Unhandled info received: ~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Private functions
handle_command(#nick_command{nick_name = NickName}, State=#state{username = ""}) -> {ok, State#state{nick = NickName}};
handle_command(#nick_command{nick_name = _}, State=#state{logged_in = true}) -> {ok, State};
handle_command(#user_command{user_name = Username}, State=#state{nick = ""}) -> {ok, State#state{username = Username}};

handle_command(#nick_command{nick_name = NickName}, State=#state{}) ->
  send_welcome_message(State#state.socket, NickName),
  register_user_process(NickName),
  {ok, State#state{nick = NickName, logged_in = true}};

handle_command(#user_command{user_name = Username}, State=#state{logged_in = false}) ->
  send_welcome_message(State#state.socket, State#state.nick),
  register_user_process(State#state.nick),
  {ok, State#state{username = Username, logged_in = true}};

%% Ignore non-NICK/USER commands when not logged in
handle_command(Command, State=#state{logged_in = false}) ->
  io:format("Command received while not logged in: ~p~n", [Command]),
  {ok, State};

handle_command(#join_command{channels = []}, State) -> {ok, State};
handle_command(#join_command{channels = [Channel|Rest]}, State) ->
  case irc_channel_manager_server:join_channel(Channel, State#state.username) of
    already_in_channel -> ok;
    ok ->
      {ok, #channel_details{name = Channel, topic = Topic}} = irc_channel_manager_server:get_channel_details(Channel),
      send(State#state.socket, #channel_topic{sender = get_server_mask(), channel = Channel, topic = Topic}),
      ok
  end,

  handle_command(#join_command{channels = Rest}, State);

handle_command(#part_command{channels = []}, State) -> {ok, State};
handle_command(#part_command{channels = [Channel|Rest]}, State) ->
  case irc_channel_manager_server:leave_channel(Channel, State#state.username) of
    not_in_channel -> ok;
    ok ->
      send(State#state.socket, #user_parted_channel{sender = State#state.nick, channel = Channel}),
      ok
  end,

  handle_command(#part_command{channels = Rest}, State);

handle_command(#priv_msg_command{target = Target, message = Message}, State) ->
  send_message_to_other_user(Target, Message, State),
  {ok, State};

handle_command(Command, State) ->
  io:format("Unknown command received: ~p~n", [Command]),
  {ok, State}.

send_welcome_message(Socket, Nickname) ->
  Message = #welcome_message{sender = get_server_mask(), nickname = Nickname, message = "Welcome to Sis-Erlang"},
  send(Socket, Message).

get_server_mask() ->
  {ok, Hostname} = inet:gethostname(),
  Hostname.

send(Socket, Message) when is_list(Message) ->
  io:format("Sending ~s~n", [Message]),
  gen_tcp:send(Socket, Message);

send(Socket, Message) ->
  String = irc_sent_messages:get_string(Message) ++ "\r\n",
  io:format("Sending ~s~n", [String]),
  gen_tcp:send(Socket, String).

register_user_process(NickName) ->
  ok = pg2:create(?ProcessGroup(NickName)),
  ok = pg2:join(?ProcessGroup(NickName), self()).

unregister_user_process(NickName) ->
  pg2:leave(?ProcessGroup(NickName), self()).

send_message_to_other_user(RecipientNick, MessageContent, SenderState=#state{}) ->
  io:format("Looking for user process ~p~n", [?ProcessGroup(RecipientNick)]),

  case pg2:get_members(?ProcessGroup(RecipientNick)) of
    {error, {no_such_group, _}} ->
      send(SenderState#state.socket,
        #no_such_nick{
          sender = get_server_mask(),
          recipient_nick = SenderState#state.nick,
          failed_nick = RecipientNick});

    [] ->
      send(SenderState#state.socket,
        #no_such_nick{
          sender = get_server_mask(),
          recipient_nick = SenderState#state.nick,
          failed_nick = RecipientNick});

    [Pid|_] ->
      UserMask = SenderState#state.nick,
      Message = #private_message{sender = UserMask, recipient = RecipientNick, message = MessageContent},
      StringMessage = irc_sent_messages:get_string(Message),
      Pid ! #user_message{raw_message = StringMessage}
  end.
