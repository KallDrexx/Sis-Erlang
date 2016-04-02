-module(irc_command).
-include_lib("irc_server/include/irc_commands.hrl").

-record(raw_command, {command="", tail=""}).

%% API
-export([parse/1]).

parse(String) when is_list(String) ->
  RawCommand = get_raw_command(String, []),
  form_command(RawCommand).

get_raw_command([], Acc) -> #raw_command{command = Acc, tail = ""};
get_raw_command([Letter], Acc) -> #raw_command{command = lists:reverse([Letter|Acc])};
get_raw_command([32|Rest], Acc) -> #raw_command{command = lists:reverse(Acc), tail = Rest};
get_raw_command([Letter|Rest], Acc) -> get_raw_command(Rest, [Letter|Acc]).

form_command(#raw_command{command = "NICK", tail = Nick}) -> #nick_command{nick_name = Nick};
form_command(#raw_command{command = "USER", tail = Arguments}) -> get_user_command(string:tokens(Arguments, " "));
form_command(#raw_command{command = "JOIN", tail = Arguments}) -> get_join_command(string:tokens(Arguments, ","), []);
form_command(#raw_command{command = "PART", tail = Arguments}) -> get_part_command(string:tokens(Arguments, ","), [], []);
form_command(#raw_command{command = "PRIVMSG", tail = Arguments}) -> extract_priv_message(Arguments, []);
form_command(#raw_command{command = "PONG", tail = _}) -> #pong_command{}.

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

extract_priv_message([], []) -> undefined;
extract_priv_message([], _TargetAcc) -> undefined;
extract_priv_message([32|Rest], TargetAcc) -> #priv_msg_command{target = lists:reverse(TargetAcc), message = Rest};
extract_priv_message([Letter|Rest], TargetAcc) -> extract_priv_message(Rest, [Letter|TargetAcc]).