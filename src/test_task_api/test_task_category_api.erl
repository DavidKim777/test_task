-module(test_task_category_api).

-export([get/1, create/1, update/1, delete/1]).

get(Body) ->
  DecodeMap = jsx:decode(Body),
  Id = maps:get(<<"id">>, DecodeMap),
  Name = maps:get(<<"Name">>, DecodeMap),
  Sport_id = maps:get(<<"Sport_id">>, DecodeMap),
  Sql = "SELECT * FROM category WHERE id = $1 AND name = $2 AND sport_id = $3",
  Params = [Id, Name, Sport_id],
  case test_task_db:query(Sql, Params) of
    {ok, _, Rows} ->
      Json = jsx:encode(#{status => <<"success">>, data => Rows}),
      {ok, Json};
    {error, Reason} ->
      lager:error("Database query failed: ~p", {Reason}),
      Json = jsx:encode(#{status => <<"error">>, message => <<"Internal server error">> }),
      {error, Json}
  end.

create(Body) ->
  DecodeMap = jsx:encode(Body),
  Name = maps:get(<<"Name">>, DecodeMap),
  SportId = maps:get(<<"SportId">>, DecodeMap),
  Sql = "INSERT INTO category(name, sport_id) VALUES ($1, $2), ROUTING id",
  Params = [Name, SportId],
  case test_task_db:query(Sql, Params) of
    {ok, _, {[Id]}} ->
      Json = jsx:encode(#{status => <<"success">>, id => Id}),
      {ok, Json};
    {error, Reason} ->
      lager:error("Database query failed: ~p", [Reason]),
      Json = jsx:encode(#{status => <<"error">>, message => <<"Internal server error">>}),
      {error, Json}
  end.

update(Body) ->
  DecodeMap = jsx:decode(Body),
  Name = maps:get(<<"Name">>, DecodeMap),
  Id = maps:get(<<"Id">>, DecodeMap),
  Sql = "UPDATE sports SET name = $1 WHERE id = $2",
  Params = [Name, Id],
  case test_task_db:query(Sql, Params) of
    {ok, Count} ->
      Json = jsx:encode(#{status => <<"success">>, data => Count}),
      {ok, Json};
    {error,Reason} ->
      lager:error("Not found: ~p", [Reason]),
      Json = jsx:encode(#{status => <<"error">>, message => <<"Not found">>}),
      {error, Json}
  end.

delete(Body) ->
  DecodeMap = jsx:decode(Body),
  Id = maps:get(<<"Id">>, DecodeMap),
  Sql = "DELETE FROM category WHERE id = $1",
  Params = [Id],
  case test_task_db:query(Sql, Params) of
    {ok, _,  [{Id}]} ->
      reply_for_rows_delete([{Id}]);
    {error, Reason} ->
      reply_error(Reason)
  end.

reply_for_rows_delete([{Id}]) ->
  Json = jsx:encode(#{message => <<"Deleted">>, id => Id}),
  {ok, Json};
reply_for_rows_delete([]) ->
  Json = jsx:encode(#{status => <<"error">>, message => <<"Not found">>}),
  {error, Json}.

reply_error(Reason) ->
  lager:error("Database query failed: ~p", [Reason]),
  Json = jsx:encode(#{status => <<"error">>, message => <<"Internal server error">>}),
  {error, Json}.