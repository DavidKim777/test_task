-module(test_task_sport_handler).

-export([init/2]).

init(Req0, State) ->
  Method = cowboy_req:method(Req0),
  Path = cowboy_req:path(Req0),
  {ok, Body, _} = cowboy_req:read_body(Req0),
  {ok, Req} = dispatch(Method, Path, Body, Req0),
  {ok, Req, State}.

dispatch(<<"POST">>, <<"/sport/get_all">>, Body, Req0) ->
  {ok, Sql, Params} = test_task_sport_api:get(Body),
  case test_task_db:equery(Sql, Params) of
    {ok, _, Rows} ->
      Json = jsx:encode(Rows),
      Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req0),
      {ok, Req};
    {error, Reason} ->
      Json = jsx:encode(#{error => Reason}),
      Req = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, Json, Req0),
      {ok, Req}
  end;
dispatch(<<"POST">>, <<"/sport/update">>, Body, Req0) ->
  {Sql, Params} = test_task_sport_api:update(Body),
  case test_task_db:equery(Sql, Params) of
    {ok, Count} ->
      Json = jsx:encode(Count),
      Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req0),
      {ok, Req};
    {error, Reason} ->
      Json1 = jsx:encode(#{error => Reason}),
      Req = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, Json1, Req0),
      {ok, Req}
  end;
dispatch(<<"POST">>, <<"/sport/create">>, Body, Req0) ->
  {Sql, Params} = test_task_sport_api:create(Body),
  case test_task_db:equery(Sql, Params) of
    {ok, _, [{Id}]} ->
      Json = jsx:encode(#{id => Id}),
      Req = cowboy_req:reply(201, #{<<"content-type">> => <<"application/json">>}, Json, Req0),
      {ok, Req};
    {error, Reason} ->
      Json1 = jsx:encode(#{error => Reason}),
      Req = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, Json1, Req0),
      {ok, Req}
  end;
dispatch(<<"POST">>, <<"/sport/delete">>, Body, Req0) ->
  {Sql, Params} = test_task_sport_api:delete(Body),
  case test_task_db:equery(Sql, Params) of
    {ok, _, Rows} ->
      reply_for_rows_delete(Rows, Req0);
    {error, Reason} ->
      reply_error(Reason, Req0)
  end.


reply_for_rows_delete(Req0, [{Id}]) ->
  Json = jsx:encode(#{message => <<"Deleted">>, id => Id}),
  Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req0),
  {ok, Req};
reply_for_rows_delete(Req0, []) ->
  Json = jsx:encode(#{error => <<"Not found">>}),
  Req = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, Json, Req0),
  {ok, Req}.

reply_error(Req0, {error, Reason}) ->
  Json1 = jsx:encode(#{error => Reason}),
  Req = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, Json1, Req0),
  {ok, Req}.