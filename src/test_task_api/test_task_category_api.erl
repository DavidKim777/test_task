-module(test_task_category_api).

-export([get/1, create/1, update/1, delete/1]).

get(DecodeMap) ->
  Id = maps:get(<<"id">>, DecodeMap),
  Name = maps:get(<<"Name">>, DecodeMap),
  Sport_id = maps:get(<<"Sport_id">>, DecodeMap),
  Sql = "SELECT * FROM category WHERE id = $1 AND name = $2 AND sport_id = $3",
  Params = [binary_to_list(Id), binary_to_list(Name), binary_to_list(Sport_id)],
  {Sql, Params}.

create(DecodeMap) ->
  Name = maps:get(<<"Name">>, DecodeMap),
  SportId = maps:get(<<"SportId">>, DecodeMap),
  Sql = "INSERT INTO category(name, sport_id) VALUES ($1, $2), ROUTING id",
  Params = [binary_to_list(Name), binary_to_list(SportId)],
  {ok, Sql, Params}.

update(DecodeMap) ->
  Name = maps:get(<<"Name">>, DecodeMap),
  Id = maps:get(<<"Id">>, DecodeMap),
  Sql = "UPDATE sports SET name = $1 WHERE id = $2",
  Params = [binary_to_list(Name), binary_to_list(Id)],
  {Sql, Params}.

delete(DecodeMap) ->
  Id = maps:get(<<"Id">>, DecodeMap),
  Sql = "DELETE FROM category WHERE id = $1",
  Params = [binary_to_list(Id)],
  {ok, Sql, Params}.