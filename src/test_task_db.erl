-module(test_task_db).

-export([start_link/0, query/1]).

start_link() ->
  {ok, Pid} = epgsql:connect(#{
    host => "localhost",
    username => "postgres",
    password => "postgres",
    database => "linein_db"
  }),
  Pid.

query(Sql) ->
  {ok, Conn} = poolboy:checkout(db_pool),
  try
    {ok, Result} = epgsql:squery(Conn, Sql),
    Result
  after
    poolboy:checkin(db_pool, Conn)
  end.