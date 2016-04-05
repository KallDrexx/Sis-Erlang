-module(irc_channel_manager_server).
-include_lib("irc_server/include/irc_channel.hrl").
-behavior(gen_server).

-record(state, {table}).

%% API
-export([start_link/0, stop/0, join_channel/2, leave_channel/2, get_channel_details/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:stop(?MODULE).

join_channel(ChannelName, User) -> gen_server:call(?MODULE, {join, {ChannelName, User}}).
leave_channel(ChannelName, User) -> gen_server:call(?MODULE, {leave, {ChannelName, User}}).
get_channel_details(ChannelName) -> gen_server:call(?MODULE, {get_details, ChannelName}).

init(_) ->
  Table = ets:new(?MODULE, [set, named_table, {keypos, #channel_details.name}]),
  {ok, #state{table = Table}}.

handle_call({join, {ChannelName, User}}, _From, State=#state{table = Table}) ->
  Reply = case ets:lookup(Table, ChannelName) of
    [] -> add_user_to_channel(User, ChannelName, Table);
    [Channel=#channel_details{}] -> add_user_to_channel(User, Channel, Table)
  end,

  {reply, Reply, State};

handle_call({get_details, ChannelName}, _From, State=#state{table = Table}) ->
  case ets:lookup(Table, ChannelName) of
    [Channel=#channel_details{}] -> {reply, {ok, Channel}, State};
    [] -> {reply, {ok, #channel_details{name = ChannelName}}, State}
  end;

handle_call({leave, {ChannelName, User}}, _From, State=#state{table = Table}) ->
  Reply = case ets:lookup(Table, ChannelName) of
    [Channel=#channel_details{}] -> remove_user_from_channel(User, Channel, Table);
    [] -> not_in_channel
  end,

  {reply, Reply, State}.

handle_cast(_Request, _State) ->
  erlang:error(not_implemented).

handle_info(Info, State) ->
  io:format("Unexpected message to channel manager server: ~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Internal functions
remove_user_from_channel(User, Channel=#channel_details{users = UserMap}, Table) ->
  case maps:find(User, UserMap) of
    error -> not_in_channel;
    {ok, _} ->
      true = ets:insert(Table, Channel#channel_details{users = maps:remove(User, UserMap)}),
      ok
  end.

add_user_to_channel(User, Channel=#channel_details{users = UserMap}, Table) ->
  case maps:find(User, UserMap) of
    {ok, _} -> already_in_channel;
    error ->
      true = ets:insert(Table, Channel#channel_details{users = maps:put(User, #channel_user{id = User}, UserMap)}),
      ok
  end;

add_user_to_channel(User, ChannelName, Table) -> %% Called when no channel exists
  DefaultTopic = lists:flatten(["Default topic for "|ChannelName]),
  ChannelDetails = #channel_details{name = ChannelName, topic = DefaultTopic, users = #{User => #channel_user{id = User}}},
  true = ets:insert(Table, ChannelDetails),
  ok.