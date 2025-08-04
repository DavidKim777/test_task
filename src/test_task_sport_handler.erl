-module(test_task_sport_handler).

-export([init/2]).

init(Req0, State) ->
  Method = cowboy_req:method(Req0),
  Path = cowboy_req:path(Req0),
  {ok, Body, _} = cowboy_req:read_body(Req0),
  {struct, DecodeMap}= jsx:decode(Body),
  {ok, StatusCode, Map} = dispatch(Method, Path, DecodeMap),
  Json = jsx:encode(Map),
  Headers = #{<<"content-type">> => <<"application/json">>},
  Req = cowboy_req:reply(StatusCode, Headers, Json, Req0),
  {ok, Req, State}.

dispatch(<<"POST">>, <<"/sport/get_all">>, DecodeMap) ->
  {ok, Sql, Params} = test_task_sport_api:get(DecodeMap),
  case test_task_db:query(Sql, Params) of
    {ok, _, Rows} ->
      {ok, 200, #{status => <<"success">>, data => Rows}};
    {error, Reason} ->
      lager:error("Database query failed: ~p", [Reason]),
      {ok, 500, #{status => <<"error">>, message => <<"Internal server error">>}}
  end;
dispatch(<<"POST">>, <<"/sport/update">>, DecodeMap) ->
  {ok, Sql, Params} = test_task_sport_api:update(DecodeMap),
  case test_task_db:query(Sql, Params) of
    {ok, Count} ->
      {ok, 200, #{status => <<"success">>, data => Count}};
    {error, Reason} ->
      lager:error("Not found: ~p", [Reason]),
      {ok, 404, #{status => <<"error">>, message => "Not found"}}
  end;
dispatch(<<"POST">>, <<"/sport/create">>, DecodeMap) ->
  {ok, Sql, Params} = test_task_sport_api:create(DecodeMap),
  case test_task_db:query(Sql, Params) of
    {ok, _, [{Id}]} ->
      {ok, 200, #{status => <<"success">>, data => [{Id}]}};
    {error, Reason} ->
      lager:error("Database query failed: ~p", [Reason]),
      {ok, 500, #{status => <<"error">>, message => <<"Internal server error">>}}
  end;
dispatch(<<"POST">>, <<"/sport/delete">>, DecodeMap) ->
  {ok, Sql, Params} = test_task_sport_api:delete(DecodeMap),
  case test_task_db:query(Sql, Params) of
    {ok, _, [{Id}]} ->
      reply_for_rows_delete(Id);
    {error, Reason} ->
      reply_error(Reason)
  end;
dispatch(<<"POST">>, <<"/category/get_all">>, DecodeMap) ->
  {ok, Sql, Params} = test_task_category_api:get(DecodeMap),
  case test_task_db:query(Sql, Params) of
    {ok, _, Rows} ->
      {ok, 200, #{status => <<"success">>, data => Rows}};
    {error, Reason} ->
      lager:error("Database query failed: ~p", {Reason}),
      {ok, 500, #{status => <<"error">>, message => <<"Internal server error">> }}
  end;
dispatch(<<"POST">>, <<"/category/create">>, DecodeMap) ->
  {ok, Sql, Params} = test_task_category_api:create(DecodeMap),
  case test_task_db:query(Sql, Params) of
    {ok, _, {[Id]}} ->
      {ok, 200, #{status => <<"success">>, id => Id}};
    {error, Reason} ->
      lager:error("Database query failed: ~p", [Reason]),
      {ok, 500,#{status => <<"error">>, message => <<"Internal server error">>}}
  end;
dispatch(<<"POST">>, <<"/category/update">>, DecodeMap) ->
  {ok, Sql, Params} = test_task_category_api:update(DecodeMap),
  case test_task_db:query(Sql, Params) of
    {ok, Count} ->
      {ok, 200, #{status => <<"success">>, data => Count}};
    {error,Reason} ->
      lager:error("Not found: ~p", [Reason]),
      {ok, 404, #{status => <<"error">>, message => <<"Not found">>}}
  end;
dispatch(<<"POST">>, <<"/category/delete">>, DecodeMap) ->
  {ok, Sql, Params} = test_task_category_api:delete(DecodeMap),
  case test_task_db:query(Sql, Params) of
    {ok, _,  [{Id}]} ->
      reply_for_rows_delete([{Id}]);
    {error, Reason} ->
      reply_error(Reason)
  end.


reply_for_rows_delete([{Id}]) ->
  {ok, 200, #{message => <<"Deleted">>, id => Id}};
reply_for_rows_delete([]) ->
  {ok, 404, #{status => <<"error">>, message => <<"Not found">>}}.

reply_error(Reason) ->
  lager:error("Database query failed: ~p", [Reason]),
  {ok, 500, #{status => <<"error">>, message => <<"Internal server error">>}}.