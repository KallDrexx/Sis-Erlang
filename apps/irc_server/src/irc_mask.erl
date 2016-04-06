-module(irc_mask).
-include_lib("irc_server/include/irc_masks.hrl").

%% API
-export([form/3, parse/1]).

form(NickName, UserId, {Ip1, Ip2, Ip3, Ip4}) ->
  lists:flatten([NickName, "!", UserId, "@",
    integer_to_list(Ip1), ".", integer_to_list(Ip2), ".",
    integer_to_list(Ip3), ".", integer_to_list(Ip4)]);

form(NickName, UserId, Host) -> lists:flatten([NickName, "!", UserId, "@", Host]).

parse(Mask) when is_list(Mask) -> internal_parse(string:tokens(Mask, "!@")).

internal_parse([Nickname, Username, Host]) ->
  #mask_parts{nick = Nickname, host = Host, username = Username}.