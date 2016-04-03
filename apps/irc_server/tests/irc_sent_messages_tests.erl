-module(irc_sent_messages_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("irc_server/include/irc_server_messages.hrl").

can_get_string_for_welcome_message_test() ->
  Message = #welcome_message{sender = "server", nickname = "Nickname", message = "Message"},
  ?assertMatch(":server 001 Nickname Message", irc_sent_messages:get_string(Message)).

can_get_string_for_topic_message_test() ->
  Message = #channel_topic{sender = "server", recipient_nick = "recipient", channel = "chan", topic = "topic"},
  ?assertMatch(":server 332 recipient chan :topic", irc_sent_messages:get_string(Message)).

can_get_string_for_channel_user_list_test() ->
  Message = #chan_user_list{sender = "server", recipient_nick = "recipient", channel = "chan", users = ["Me1", "Me2"]},
  ExpectedResult = ":server 353 recipient @ chan :recipient +Me1 +Me2",
  ?assertMatch(ExpectedResult, irc_sent_messages:get_string(Message)).

can_get_string_for_channel_user_list_end_test() ->
  Message = #chan_user_list_end{sender = "server", recipient_nick = "recipient", channel = "chan"},
  ExpectedResult = ":server 366 recipient chan :End of /NAMES list.",
  ?assertMatch(ExpectedResult, irc_sent_messages:get_string(Message)).

%% Error Messages
can_get_string_for_not_enough_params_test() ->
  Message = #not_enough_params{sender = "server", recipient_nick = "recipient", command = "command"},
  ExpectedResult = ":server 461 recipient command :Not enough parameters",
  ?assertMatch(ExpectedResult, irc_sent_messages:get_string(Message)).

can_get_string_for_nickname_in_use_test() ->
  Message = #nickname_in_use{sender = "server", recipient_nick = "recipient", failed_nick = "bad_nick"},
  ExpectedResult = ":server 433 recipient bad_nick :Nickname is already in use",
  ?assertMatch(ExpectedResult, irc_sent_messages:get_string(Message)).

can_get_string_for_no_such_channel_test() ->
  Message = #no_such_channel{sender = "server", recipient_nick = "recipient", channel = "chan"},
  ExpectedResult = ":server 403 recipient chan :No such channel",
  ?assertMatch(ExpectedResult, irc_sent_messages:get_string(Message)).

can_get_string_for_no_such_nick_test() ->
  Message = #no_such_nick{sender = "server", recipient_nick = "recipient", failed_nick = "bad_nick"},
  ExpectedResult = ":server 401 recipient bad_nick :No such nick/channel",
  ?assertMatch(ExpectedResult, irc_sent_messages:get_string(Message)).

%% Announcements
can_get_string_for_private_message_announcement_test() ->
  Message = #private_message{sender = "sender", recipient = "recipient", message = "message"},
  ExpectedResult = ":sender PRIVMSG recipient :message",
  ?assertMatch(ExpectedResult, irc_sent_messages:get_string(Message)).

can_get_string_for_user_joined_channel_announcement_test() ->
  Message = #user_joined_channel{sender = "sender", channel = "chan"},
  ExpectedResult = ":sender JOIN chan",
  ?assertMatch(ExpectedResult, irc_sent_messages:get_string(Message)).

can_get_string_for_user_parted_channel_announcement_test() ->
  Message = #user_parted_channel{sender = "sender", channel = "chan"},
  ExpectedResult = ":sender PART chan",
  ?assertMatch(ExpectedResult, irc_sent_messages:get_string(Message)).