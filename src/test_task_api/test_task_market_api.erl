-module(test_task_market_api).

-export([get/1, create/1, update/1, delete/1]).

get(DecodeMap) ->
  Id = maps:get(<<"Id">>, DecodeMap),
  Name = maps:get(<<"Name">>, DecodeMap),
  EventId = maps:get(<<"Event_id">>, DecodeMap),
  Sql = "SELECT * FROM marcet WHERE id = $1 AND name = $2 AND event_id = $3",
  Params = [Id, Name, EventId],
  case test_task_db:query(Sql, Params) of
    {ok, _, Rows} ->
      Map = #{status => <<"success">>, data => Rows},
      {ok, Map};
    {error, Reason} ->
      lager:error("Database query failed: ~p", [Reason]),
      Map = #{status => <<"error">>, message => <<"Internal server error">>},
      {error, Map}
  end.

create(DecodeMap) ->
  Name = maps:get(<<"Name">>, DecodeMap),
  EventId = maps:get(<<"Event_id">>, DecodeMap),
  Sql = "INSERT INTO market(name, event_id) VALUES ($1, $2) ROUTING id",
  Params = [Name, EventId],
  case test_task_db:query(Sql, Params) of
    {ok, _, [{Id}]} ->
      Map = #{status => <<"success">>, data => Id},
      {ok, Map};
    {error, Reason} ->
      lager:error("Database query failed: ~p", [Reason]),
      Map = #{status => <<"error">>, message => <<"Instal server error">>},
      {error, Map}
  end.

update(DecodeMap) ->
  Id = maps:get(<<"Id">>, DecodeMap),
  Name = maps:get(<<"Name">>, DecodeMap),
  Sql = "UPDATE sports SET name = $1 WHERE id = $2",
  Params = [Id, Name],
  case test_task_db:query(Sql, Params) of
    {ok, Count} ->
      Map = #{status => <<"success">>, data => Count},
      {ok, Map};
    {error,Reason} ->
      lager:error("Not found: ~p", [Reason]),
      Map = #{status => <<"error">>, message => <<"Not found">>},
      {error, Map}
  end.

delete(DecodeMap) ->
  Id = maps:get(<<"Id">>, DecodeMap),
  Sql = "DELETE FROM sports WHERE id = $1",
  Params = [Id],
  case test_task_db:query(Sql, Params) of
    {ok, _, [{Id}]} ->
      reply_for_rows_delete(Id);
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
