-module(irc_sent_messages).
-include_lib("irc_server/include/irc_server_messages.hrl").

%% API
-export([get_string/1]).

%% Replies
get_string(#welcome_message{sender = Address, nickname = Nickname, message = Message}) ->
  lists:flatten([":", Address, " 001 ", Nickname, " ", Message]);

get_string(#channel_topic{sender = Address, recipient_nick = Recipient, channel = Channel, topic = Topic}) ->
  lists:flatten([":", Address, " 332 ", Recipient, " ", Channel, " :", Topic]);

get_string(#chan_user_list{sender = Address, recipient_nick = Recipient, channel = Channel, users = Users}) ->
  FlattenedUsers = extract_user_list_for_chan_list_message(Users, []),
  lists:flatten([":", Address, " 353 ", Recipient, " @ ", Channel, " :", Recipient, FlattenedUsers]);

get_string(#chan_user_list_end{sender = Address, recipient_nick = Recipient, channel = Channel}) ->
  lists:flatten([":", Address, " 366 ", Recipient, " ", Channel, " :End of /NAMES list."]);

%% Errors
get_string(#not_enough_params{sender = Address, recipient_nick = Recipient, command = Command}) ->
  lists:flatten([":", Address, " 461 ", Recipient, " ", Command, " :Not enough parameters"]);

get_string(#nickname_in_use{sender = Address, recipient_nick = Recipient, failed_nick = BadNick}) ->
  lists:flatten([":", Address, " 433 ", Recipient, " ", BadNick, " :Nickname is already in use"]);

get_string(#no_such_channel{sender = Address, recipient_nick = Recipient, channel = ChannelName}) ->
  lists:flatten([":", Address, " 403 ", Recipient, " ", ChannelName, " :No such channel"]);

get_string(#no_such_nick{sender = Address, recipient_nick = Recipient, failed_nick = BadNick}) ->
  lists:flatten([":", Address, " 401 ", Recipient, " ", BadNick, " :No such nick/channel"]);

get_string(#private_message{sender = Address, recipient = Recipient, message = Message}) ->
  lists:flatten([":", Address, " PRIVMSG ", Recipient, " :", Message]);

get_string(#user_joined_channel{sender = Address, channel = Channel}) ->
  lists:flatten([":", Address, " JOIN ", Channel]);

get_string(#user_parted_channel{sender = Address, channel = Channel}) ->
  lists:flatten([":", Address, " PART ", Channel]).

extract_user_list_for_chan_list_message([], Acc) -> lists:flatten(lists:reverse(Acc));
extract_user_list_for_chan_list_message([User|Rest], Acc) ->
  extract_user_list_for_chan_list_message(Rest, [" +" ++ User | Acc]).