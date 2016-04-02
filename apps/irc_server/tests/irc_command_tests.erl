-module(irc_command_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("irc_server/include/irc_commands.hrl").

can_parse_nick_command_test() ->
  ?assertMatch(#nick_command{nick_name = "Username"}, irc_command:parse("NICK Username")).

can_parse_user_command_test() ->
  RawString = "USER username hostname servername realname",
  Command = #user_command{
    user_name = "username",
    host_name = "hostname",
    server_name = "servername",
    real_name = "realname"
  },
  ?assertMatch(Command, irc_command:parse(RawString)).

user_command_returns_undefined_if_less_than_4_arguments_provided_test() ->
  ?assertMatch(undefined, irc_command:parse("USER username hostname servername")).

can_parse_join_command_with_single_channel_test() ->
  ?assertMatch(#join_command{channels = ["channel"]}, irc_command:parse("JOIN channel")).

can_parse_join_command_with_multiple_channels_test() ->
  ?assertMatch(#join_command{channels = ["c1","c2"]}, irc_command:parse("JOIN c1,c2")).

can_parse_part_command_with_single_channel_test() ->
  ?assertMatch(#part_command{channels = ["channel"], message = "message"}, irc_command:parse("PART channel message")).

can_part_part_command_with_multiple_channels_test() ->
  ?assertMatch(#part_command{channels = ["c1","c2"], message = "message"}, irc_command:parse("PART c1,c2 message")).

can_parse_private_message_command_test() ->
  ?assertMatch(#priv_msg_command{target = "dest", message = "message"}, irc_command:parse("PRIVMSG dest message")).

returns_undefined_if_private_message_does_not_have_target_and_message_test() ->
  ?assertMatch(undefined, irc_command:parse("PRIVMSG dest")).

can_parse_pong_command_test() ->
  ?assertMatch(#pong_command{}, irc_command:parse("PONG")).

unknown_command_returns_unknown_command_record_test() ->
  ?assertMatch(#unknown_command{raw_command = "omg test"}, irc_command:parse("omg test")).

can_parse_quit_command_test() ->
  ?assertMatch(#quit_command{message = "good bye!"}, irc_command:parse("QUIT good bye!")).