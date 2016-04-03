%% Replies
-record(welcome_message, {sender="", nickname="", message=""}).
-record(channel_topic, {sender="", recipient_nick="", channel="", topic=""}).
-record(chan_user_list, {sender="", recipient_nick="", channel="", users=[]}).
-record(chan_user_list_end, {sender="", recipient_nick="", channel=""}).

%% Errors
-record(not_enough_params, {sender="", recipient_nick="", command=""}).
-record(nickname_in_use, {sender="", recipient_nick="", failed_nick=""}).
-record(no_such_channel, {sender="", recipient_nick="", channel=""}).
-record(no_such_nick, {sender="", recipient_nick="", failed_nick=""}).

%% Announcements
-record(private_message, {sender="", recipient="", message=""}).
-record(user_joined_channel, {sender="", channel=""}).
-record(user_parted_channel, {sender="", channel=""}).