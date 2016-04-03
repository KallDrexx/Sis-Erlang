-module(irc_mask).
-include_lib("irc_server/include/irc_masks.hrl").

%% API
-export([form/3, parse/1]).

form(NickName, UserId, Host) -> lists:flatten([NickName, "!", UserId, "@", Host]).

parse(Mask) when is_list(Mask) -> internal_parse(string:tokens(Mask, "!@")).

internal_parse([Nickname, Username, Host]) ->
  #mask_parts{nick = Nickname, host = Host, username = Username}.