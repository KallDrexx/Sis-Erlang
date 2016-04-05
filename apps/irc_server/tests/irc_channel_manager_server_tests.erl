-module(irc_channel_manager_server_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("irc_server/include/irc_channel.hrl").

single_user_can_join_channel_test() ->
  start(),
  ok = irc_channel_manager_server:join_channel("name", "user"),
  ?assertMatch({ok, #channel_details{name = "name", users = #{"user" := _}}}, irc_channel_manager_server:get_channel_details("name")).

can_leave_channel_test() ->
  start(),
  ok = irc_channel_manager_server:join_channel("name", "user"),
  ok = irc_channel_manager_server:leave_channel("name", "user"),
  ?assertMatch({ok, #channel_details{name = "name", users = #{}}}, irc_channel_manager_server:get_channel_details("name")).

empty_channel_details_when_getting_details_of_channel_that_does_not_exist_test() ->
  start(),
  ?assertMatch({ok, #channel_details{name = "name", users = #{}}}, irc_channel_manager_server:get_channel_details("name")).

leaving_channel_user_was_never_in_test() ->
  start(),
  ok = irc_channel_manager_server:join_channel("name", "user"),
  ?assertMatch(not_in_channel, irc_channel_manager_server:leave_channel("name", "user2")).

leaving_channel_that_never_existed_test() ->
  start(),
  ?assertMatch(not_in_channel, irc_channel_manager_server:leave_channel("name", "user2")).

second_user_can_join_channel_test() ->
  start(),
  ok = irc_channel_manager_server:join_channel("name", "user"),
  ok = irc_channel_manager_server:join_channel("name", "user2"),
  ?assertMatch({ok, #channel_details{name = "name", users = #{"user" := _, "user2" := _}}},
    irc_channel_manager_server:get_channel_details("name")).

joining_a_table_a_second_time_test() ->
  start(),
  ok = irc_channel_manager_server:join_channel("name", "user"),
  ?assertMatch(already_in_channel, irc_channel_manager_server:join_channel("name", "user")).

%% Setup functions
start() ->
  catch exit(irc_channel_manager_server:stop()), %% Make sure channel manager is stopped before each test run
  irc_channel_manager_server:start_link().
