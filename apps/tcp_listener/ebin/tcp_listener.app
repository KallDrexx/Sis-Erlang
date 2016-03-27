{application, tcp_listener, [
  {description, "Application to listen for tcp connections"},
  {vsn, "1"},
  {registered, []},
  {mod, {tcp_listener, []}},
  {env, [
    {port, 9090},
    {acceptor_pool_size, 20},
    {on_accept_mfa, {tcp_listener_echo_server, start, []}}
  ]}
]}.