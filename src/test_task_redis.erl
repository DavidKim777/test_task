-module(test_task_redis).

-export([start_link/0, set_cache/3, get_cache/2]).

start_link() ->
  {ok, Pid} = eredis:start_link("127.0.0.1", 6379, []),
  Pid.
set_cache(Pid, Key, Value) ->
  eredis:q(Pid, ["SET", Key, Value]).


get_cache(Pid, Key) ->
  case eredis:q(Pid, ["GET", Key]) of
    {ok, undefined} -> not_found;
    {ok, Value} -> {ok, Value};
    {error, Reason} -> {error, Reason}
  end.