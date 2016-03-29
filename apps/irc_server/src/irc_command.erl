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

form_command(#raw_command{command = "NICK", tail = Tail}) ->  #nick_command{nickname = Tail}.