-record(nick_command, {nick_name=""}).
-record(user_command, {user_name="", host_name="", server_name="", real_name=""}).
-record(join_command, {channels=[]}).
-record(part_command, {channels=[], message=""}).