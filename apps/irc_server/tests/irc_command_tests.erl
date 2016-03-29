-module(irc_command_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("irc_server/include/irc_commands.hrl").

can_parse_nick_command_test() ->
  ?assertMatch(#nick_command{nickname = "Username"}, irc_command:parse("NICK Username")).