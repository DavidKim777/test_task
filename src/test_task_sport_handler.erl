-module(test_task_sport_handler).

-export([init/2]).

init(Req0, State) ->
  Method = cowboy_req:method(Req0),
  Path = cowboy_req:path(Req0),
  {ok, Body, _} = cowboy_req:read_body(Req0),
  {ok, Req} = dispatch(Method, Path, Body, Req0),
  {ok, Req, State}.

dispatch(<<"POST">>, <<"/sport/get_all">>, Body, Req0) ->
  {struct, DecodeMap} = jsx:decode(Body),
  {ok, Sql, _} = test_task_sport_api:get(DecodeMap),
  case test_task_db:equery(Sql) of
    {ok, _, Rows} ->
      Json = jsx:encode(#{status => <<"success">>, data => Rows}),
      Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req0),
      {ok, Req};
    {error, Reason} ->
      lager:error("Database query failed: ~p", [Reason]),
      Json = jsx:encode(#{status => <<"error">>, message => <<"Internal server error">>}),
      Req = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, Json, Req0),
      {ok, Req}
  end;
dispatch(<<"POST">>, <<"/sport/update">>, Body, Req0) ->
  {struct, DecodeMap}= jsx:decode(Body),
  {Sql, _} = test_task_sport_api:update(DecodeMap),
  case test_task_db:query(Sql) of
    {ok, Count} ->
      Json = jsx:encode(#{status => <<"success">>, data => Count}),
      Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req0),
      {ok, Req};
    {error, Reason} ->
      lager:error("Not found: ~p", [Reason]),
      Json = jsx:encode(#{status => <<"error">>, message => "Not found"}),
      Req = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, Json, Req0),
      {ok, Req}
  end;
dispatch(<<"POST">>, <<"/sport/create">>, Body, Req0) ->
  {struct, DecodeMap}= jsx:decode(Body),
  {Sql, _} = test_task_sport_api:create(DecodeMap),
  case test_task_db:query(Sql) of
    {ok, _, [{Id}]} ->
      Json = jsx:encode(#{status => <<"success">>, id => Id}),
      Req = cowboy_req:reply(201, #{<<"content-type">> => <<"application/json">>}, Json, Req0),
      {ok, Req};
    {error, Reason} ->
      lager:error("Database query failed: ~p", [Reason]),
      Json = jsx:encode(#{status => <<"error">>, message => <<"Internal server error">>}),
      Req = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, Json, Req0),
      {ok, Req}
  end;
dispatch(<<"POST">>, <<"/sport/delete">>, Body, Req0) ->
  {struct, DecodeMap}= jsx:decode(Body),
  {Sql, _} = test_task_sport_api:delete(DecodeMap),
  case test_task_db:query(Sql) of
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
  Json = jsx:encode(#{status => <<"error">>, message => <<"Not found">>}),
  Req = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, Json, Req0),
  {ok, Req}.

reply_error(Req0, {error, Reason}) ->
  lager:error("Database query failed: ~p", [Reason]),
  Json = jsx:encode(#{status => <<"error">>, message => <<"Internal server error">>}),
  Req = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, Json, Req0),
  {ok, Req}.