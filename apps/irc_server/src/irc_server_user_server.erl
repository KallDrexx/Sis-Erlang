-module(irc_server_user_server).
-behavior(gen_server).

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

init(Args) ->
  erlang:error(not_implemented).

handle_call(Request, From, State) ->
  erlang:error(not_implemented).

handle_cast(Request, State) ->
  erlang:error(not_implemented).

handle_info(Info, State) ->
  erlang:error(not_implemented).

terminate(Reason, State) ->
  erlang:error(not_implemented).

code_change(OldVsn, State, Extra) ->
  erlang:error(not_implemented).