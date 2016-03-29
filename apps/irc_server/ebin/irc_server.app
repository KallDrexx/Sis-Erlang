{application, irc_server, [
  {description, "Simple IRC server"},
  {mod, {irc_server, []}},
  {registered, [irc_server]},
  {applications, [tcp_listener, process_registry]}
]}.