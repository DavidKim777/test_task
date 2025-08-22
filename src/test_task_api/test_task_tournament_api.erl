-module(test_task_tournament_api).

-export([get/1, create/1, update/1, delete/1]).

get(DecodeMap) ->
  Id = maps:get(<<"Id">>, DecodeMap),
  Name = maps:get(<<"Name">>, DecodeMap),
  CategoryId = maps:get(<<"Category_id">>, DecodeMap),
  Sql = "SELECT * FROM tournament WHERE id = $1 AND name = $2 AND category_id = $3",
  Params = [Id, Name, CategoryId],
  RedisKey = io_lib:format("sport:~p:~s", Params),
  RedisPid = whereis(redis_pid),
  get_cache_or_db(RedisPid, RedisKey, "GET", Params, Sql).

create(DecodeMap) ->
  Name = maps:get(<<"Name">>, DecodeMap),
  CategoryId = maps:get(<<"Category_id">>, DecodeMap),
  Sql = "INSERT INTO turnament(name, category_id) VALUES ($1, $2) ROUTING id",
  Params = [Name, CategoryId],
  RedisKey = io_lib:format("sport:~p:~s", Params),
  RedisPid = whereis(redis_pid),
  case test_task_db:query(Sql, Params) of
    {ok, _, {[Id]}} ->
      Map = #{status => <<"success">>, data => Id},
      Bin = term_to_binary(Map),
      _ = set_or_update_cache(RedisPid, ["SET", RedisKey, Bin, "EX", 21600]),
      {ok, Map};
    {error, Reason} ->
      lager:error("Databas query failed: ~p", [Reason]),
      Map = #{status => <<"success">>, message => <<"Internal server error">>},
      {error, Map}
  end.

update(DecodeMap) ->
  Id = maps:get(<<"Id">>, DecodeMap),
  Name = maps:get(<<"Name">>, DecodeMap),
  Sql = "UPDATE tournament SET name = $1 WHERE id = $2",
  Params = [Id, Name],
  RedisKey = io_lib:format("sport:~p:~s", Params),
  RedisPid = whereis(redis_pid),
  case test_task_db:query(Sql, Params) of
    {ok, Count} ->
      Map = #{status => <<"success">>, data => Count},
      Bin = term_to_binary(Map),
      _ = set_or_update_cache(RedisPid, ["SET", RedisKey, Bin, "EX", 21600]),
      {ok, Map};
    {error, Reason} ->
      lager:error("Database query failed: ~p", [Reason]),
      Map = #{status => <<"error">>, message => <<"Not aund">>},
      {error, Map}
  end.

delete(DecodeMap) ->
  Id = maps:get(<<"Id">>, DecodeMap),
  Sql = "DELETE FROM tournament WHERE id = $1",
  Params = [Id],
  RedisKey = io_lib:format("sport:~p:~s", Params),
  RedisPid = whereis(redis_pid),
  _ = delete_cache(RedisPid, ["DEL", RedisKey]),
  case test_task_db:query(Sql, Params) of
     {ok, _, [{Id}]} ->
       reply_for_rows_delete([{Id}]);
     {error, Reason} ->
       reply_error(Reason)
   end.

reply_for_rows_delete([{Id}]) ->
  Map = #{message => <<"Deleted">>, id => Id},
  {ok, Map};
reply_for_rows_delete([]) ->
   Map = #{status => <<"error">>, message => <<"Not found">>},
  {error, Map}.

reply_error(Reason) ->
  lager:error("Database query failed: ~p", [Reason]),
  Map = #{status => <<"error">>, message => <<"Internal server error">>},
  {error, Map}.

get_cache_or_db(RedisPid, RedisKey, "GET", Params, Sql) ->
  case test_task_redis:query(RedisPid, ["GET", RedisKey]) of
    {ok, undefined} ->
      {ok, Map} = query_get_db(Sql, Params),
      Bin = term_to_binary(Map),
      _ = set_or_update_cache(RedisPid, ["SET", RedisKey, Bin, "EX", 21600]);
    {ok, Bin} ->
      {ok, Bin};
    {error, Reason} ->
      lager:error("Error: ~p", [Reason]),
      {error, #{status => <<"error">>, message => <<"Internal server error">>}}
  end.

query_get_db(Sql, Params) ->
  case test_task_db:query(Sql, Params) of
  {ok, _, Rows} ->
    Map = #{status => <<"success">>, data => Rows},
    {ok, Map};
  {error, Reason} ->
    lager:error("Database query failed: ~p", [Reason]),
     Map = #{status => <<"error">>, message => <<"Internal server error">>},
    {error, Map}
  end.

set_or_update_cache(RedisPid, ["SET", RedisKey, JsonResult, "EX", Ttl]) ->
  test_task_redis:query(RedisPid, ["SET", RedisKey, JsonResult, "EX", Ttl]).

delete_cache(RedisPid, ["DEL", Kay]) ->
  case test_task_redis:query(RedisPid, ["DEL", Kay]) of
    {ok, Result} ->
      {ok, Result};
    {error, undefined} ->
      {error, undefined}
  end.