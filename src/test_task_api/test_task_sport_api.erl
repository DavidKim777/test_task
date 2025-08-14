-module(test_task_sport_api).

-export([get/1, create/1, update/1, delete/1]).


get(Body) ->
  {struct, DecodeMap} = jsx:decode(Body),
  Id = maps:get(<<"Id">>, DecodeMap),
  Name = maps:get(<<"Name">>, DecodeMap),
  Sql = "SELECT * FROM sport WHERE id = $1 AND name = $2 LIMIT 100 OFFSET 100",
  Params = [Id, Name],
  RedisKey = io_lib:format("sport:~p:~s", Params),
  RedisPid = whereis(redis_pid),
  case get_cache_or_db(RedisPid, "GET", RedisKey, Params, Sql) of
    {ok, Map} ->
      Json = jsx:encode(Map),
      {ok, Json};
    {error, Map} ->
      Json = jsx:encode(Map),
      {error, Json}
  end.

create(Body) ->
  DecodeMap = jsx:decode(Body),
  Name = maps:get(<<"Name">>, DecodeMap),
  Sql = "INSERT INTO sports(name) VALUES ($1) RETURNING id",
  Params = [Name],
  case test_task_db:query(Sql, Params) of
    {ok, _, [{Id}]} ->
      Map = #{status => <<"success">>, data => Id},
      Json = jsx:encode(Map),
      {ok, Json};
    {error, Reason} ->
      lager:error("Database query failed: ~p", [Reason]),
      Map = #{status => <<"error">>, message => <<"Internal server error">>},
      Json = jsx:encode(Map),
      {error, Json}
  end.

update(Body) ->
  DecodeMap = jsx:decode(Body),
  Name = maps:get(<<"Name">>, DecodeMap),
  Id = maps:get(<<"Id">>, DecodeMap),
  Sql = "UPDATE sports SET name = $1 WHERE id = $2",
  Params = [Name, Id],
  RedisKey = io_lib:format("sport:~p:~s", Params),
  RedisPid = whereis(redis_pid),
  case test_task_db:query(Sql, Params) of
    {ok, Count} ->
      _ = set_or_update_cache(RedisPid, ["SET", RedisKey, Params, "EX", 86400]),
      Map = #{status => <<"success">>, data => Count},
      Json = jsx:encode(Map),
      {ok, Json};
    {error, Reason} ->
      lager:error("Not found: ~p", [Reason]),
      Map = #{status => <<"error">>, message => "Not found"},
      Json = jsx:encode(Map),
      {error, Json}
  end.

delete(Body) ->
  DecodeMap = jsx:decode(Body),
  Id = maps:get(<<"Id">>, DecodeMap),
  Sql = "DELETE FROM sports WHERE id = $1",
  Params = [Id],
  RedisKey = io_lib:format("sport:~p:~s", Params),
  RedisPid = whereis(redis_pid),
  case test_task_db:query(Sql, Params) of
    {ok, _, [{Id}]} ->
      _ = delete_cache(RedisPid, ["DEL", RedisKey]),
      reply_for_rows_delete(Id);
    {error, Reason} ->
      reply_error(Reason)
  end.

reply_for_rows_delete([{Id}]) ->
  Map = #{status => <<"success">>, data => Id},
  Json = jsx:encode(Map),
  {ok, Json};
reply_for_rows_delete([]) ->
  Map = #{status => <<"error">>, message => "Not found"},
  Json = jsx:encode(Map),
  {error, Json}.

reply_error(Reason) ->
  lager:error("Database query failed: ~p", [Reason]),
  Map = #{status => <<"error">>, message => <<"Internal server error">>},
  Json = jsx:encode(Map),
  {error, Json}.

get_cache_or_db(RedisPid, "GET", Kay, Params, Sql) ->
  case test_task_redis:query(RedisPid, ["GET", Kay]) of
    {ok, undefined} ->
      {ok, Map} = logics_for_query_db(Sql, Params),
      JsonResult = jsx:encode(Map),
      _ = set_or_update_cache(RedisPid, ["SET", Kay, JsonResult, "EX", 86400]),
      {ok, Map};
    {ok, BinJson} ->
      Map = jsx:decode(BinJson, [return_maps]),
      {ok, Map};
    {error, Reason} ->
      lager:error("Error: ~p", [Reason]),
      {error, #{status => <<"error">>, message => <<"Internal server error">>}}
  end.

logics_for_query_db(Sql, Params) ->
  case test_task_db:query(Sql, Params) of
    {ok, _, Rows} ->
      #{status => <<"success">>, data => Rows};
    {error, Reason} ->
      lager:error("Database query failed: ~p", [Reason]),
      #{status => <<"error">>, message => <<"Internal server error">>}
  end.

set_or_update_cache(RedisPid, ["SET", Kay, Value, "EX", Ttl]) ->
  test_task_redis:query(RedisPid, ["SET", Kay, Value, "EX", Ttl]).

delete_cache(RedisPid, ["DEL", Kay]) ->
  case test_task_redis:query(RedisPid, ["DEL", Kay]) of
    {ok, Result} ->
      {ok, Result};
    {error, undefined} ->
      {error, undefined}
  end.
