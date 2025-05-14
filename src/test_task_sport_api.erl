-module(test_task_sport_api).

-export([get/1, update/1]).


get(Body) ->
  {struct, DecodeMap}= jsx:decode(Body),
  Name = maps:get(<<"Name">>, DecodeMap),
  Sql = "SELECT * FROM sport WHERE name = $1",
  Params = [binary_to_list(Name)],
  {Sql, Params}.

update(Body) ->
  {struct, DecodeMap}= jsx:decode(Body),
  Name = maps:get(<<"Name">>, DecodeMap),
  Sql = "INSERT INTO sports(name) VALUES ($1) RETURNING id",
  Params = [binary_to_list(Name)],
  {Sql, Params}.