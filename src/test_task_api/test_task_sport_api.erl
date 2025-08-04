-module(test_task_sport_api).

-export([get/1, create/1, update/1, delete/1]).


get(DecodeMap) ->
  Id = maps:get(<<"Id">>, DecodeMap),
  Name = maps:get(<<"Name">>, DecodeMap),
  Sql = "SELECT * FROM sport WHERE id = $1 AND name = $2 LIMIT 100 OFFSET 100",
  Params = [binary_to_list(Name), binary_to_list(Id)],
  {ok, Sql, Params}.

create(DecodeMap) ->
  Name = maps:get(<<"Name">>, DecodeMap),
  Sql = "INSERT INTO sports(name) VALUES ($1) RETURNING id",
  Params = [binary_to_list(Name)],
  {ok, Sql, Params}.

update(DecodeMap) ->
  Name = maps:get(<<"Name">>, DecodeMap),
  Id = maps:get(<<"Id">>, DecodeMap),
  Sql = "UPDATE sports SET name = $1 WHERE id = $2",
  Params = [binary_to_list(Name), binary_to_list(Id)],
  {ok, Sql, Params}.

delete(DecodeMap) ->
  Id = maps:get(<<"Id">>, DecodeMap),
  Sql = "DELETE FROM sports WHERE id = $1",
  Params = [binary_to_list(Id)],
  {ok, Sql, Params}.
