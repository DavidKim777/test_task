-module(test_task_db).

-export([start_link/0, query/2]).

start_link() ->
  {ok, Pid} = epgsql:connect(#{
    host => "localhost",
    username => "postgres",
    password => "postgres",
    database => "linein_db"
  }),
  Pid.

query(Sql, Params) ->
  {ok, Conn} = poolboy:checkout(db_pool),
  try
    {ok, Result} = epgsql:equery(Conn, Sql, Params),
    Result
  after
    poolboy:checkin(db_pool, Conn)
  end.