-module(test_task_redis).

-export([start_link/0, query/2]).

start_link() ->
  {ok, Pid} = eredis:start_link("127.0.0.1", 6379, []),
  register(redis_pid, Pid),
  ok.

query(Pid, List) ->
  eredis:q(Pid, List).