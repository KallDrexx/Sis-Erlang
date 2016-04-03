-module(irc_mask_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("irc_server/include/irc_masks.hrl").

can_form_mask_test() ->
  ?assertMatch("nick!user@host", irc_mask:form("nick", "user", "host")).

can_parse_mask_test() ->
  ?assertMatch(#mask_parts{host = "host", nick = "nick", username = "user"}, irc_mask:parse("nick!user@host")).
