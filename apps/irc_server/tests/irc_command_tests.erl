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