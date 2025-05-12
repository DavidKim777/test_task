-module(test_task_db).

-export([conn/0, close_conn/1, equery/2]).

conn() ->
  application:get_env(linein_db, db),
  {ok, Connect} = epgsql:connect(#{
    host => "localhost",
    username => "postgres",
    password => "postgres",
    database => "linein_db"
  }),
  {ok, #{connect => Connect}}.

close_conn(Connect) ->
  ok = epgsql:close(Connect).

equery(Sql, Parameters) ->
  #{connect := Conn}= conn(),
  epgsql:equery(Conn, Sql, Parameters),
  close_conn(Conn).