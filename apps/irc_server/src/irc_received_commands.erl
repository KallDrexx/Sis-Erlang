-module(irc_received_commands).
-include_lib("irc_server/include/irc_received_commands.hrl").

-record(raw_command, {command="", tail=""}).

%% API
-export([parse/1]).

parse(String) when is_list(String) ->
  StrippedString = re:replace(String, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
  RawCommand = get_raw_command(string:strip(StrippedString), []),
  form_command(RawCommand).

get_raw_command([], Acc) -> #raw_command{command = Acc, tail = ""};
get_raw_command([Letter], Acc) -> #raw_command{command = lists:reverse([Letter|Acc])};
get_raw_command([32|Rest], Acc) -> #raw_command{command = lists:reverse(Acc), tail = Rest};
get_raw_command([Letter|Rest], Acc) -> get_raw_command(Rest, [Letter|Acc]).

form_command(#raw_command{command = "NICK", tail = Nick}) -> #nick_command{nick_name = Nick};
form_command(#raw_command{command = "USER", tail = Arguments}) ->
  case get_user_command(string:tokens(Arguments, " ")) of
    undefined -> #unknown_command{raw_command = lists:flatten(["USER " | Arguments])};
    X -> X
  end;

form_command(#raw_command{command = "JOIN", tail = Arguments}) -> get_join_command(string:tokens(Arguments, ","), []);
form_command(#raw_command{command = "PART", tail = Arguments}) -> get_part_command(string:tokens(Arguments, ","), [], []);
form_command(#raw_command{command = "PRIVMSG", tail = Arguments}) -> extract_priv_message(Arguments, []);
form_command(#raw_command{command = "PONG", tail = _}) -> #pong_command{};
form_command(#raw_command{command = "QUIT", tail = Arguments}) -> extract_quit_message(Arguments, []);
form_command(#raw_command{command = Command, tail = Arguments}) ->
  #unknown_command{raw_command = lists:flatten([Command, " " | Arguments])}.

get_user_command([Username, Hostname, ServerName, RealName | _]) ->
  #user_command{user_name = Username, host_name = Hostname, real_name = RealName, server_name = ServerName};

get_user_command(_) -> undefined.

get_join_command([], Acc) -> #join_command{channels = lists:reverse(Acc)};
get_join_command([Channel], Acc) -> get_join_command([], [Channel|Acc]);
get_join_command([Channel|Rest], Acc) -> get_join_command(Rest, [Channel|Acc]).

get_part_command([], Channels, _) ->
  #part_command{channels = lists:reverse(Channels), message = []};

get_part_command([LastChannel], Channels, _) ->
  {Channel, Message} = extract_part_message(LastChannel, []),
  #part_command{channels = lists:reverse([Channel|Channels]), message = Message};

get_part_command([Channel|Rest], Channels, _) ->
  get_part_command(Rest, [Channel | Channels], []).

extract_part_message([], ChannelSoFar) -> {lists:reverse(ChannelSoFar), ""};
extract_part_message([32|Rest], ChannelSoFar) -> {lists:reverse(ChannelSoFar), Rest};
extract_part_message([Letter|Rest], ChannelSoFar) -> extract_part_message(Rest, [Letter|ChannelSoFar]).

extract_priv_message([], []) -> #unknown_command{raw_command = "PRIVMSG"};
extract_priv_message([], TargetAcc) -> #unknown_command{raw_command = lists:flatten(["PRIVMSG ", lists:reverse(TargetAcc)])};
extract_priv_message([32|Rest], TargetAcc) -> #priv_msg_command{target = lists:reverse(TargetAcc), message = Rest};
extract_priv_message([Letter|Rest], TargetAcc) -> extract_priv_message(Rest, [Letter|TargetAcc]).

extract_quit_message([], []) -> #quit_command{};
extract_quit_message([], Acc) -> #quit_command{message = lists:reverse(Acc)};
extract_quit_message([Letter|Rest], Acc) -> extract_quit_message(Rest, [Letter|Acc]).