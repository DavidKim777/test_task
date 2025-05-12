-module(test_task_sport_api).

-export([get/2, update/2]).


get(Body, Req0) ->
  {struct, DecodeMap}= jsx:decode(Body),
  Name = maps:get(<<"Name">>, DecodeMap),
  Sql = "SELECT * FROM sport WHERE name = $1",
  Params = [binary_to_list(Name)],
  case linein_db:equery(Sql, Params) of
    {ok, _, Rows} ->
      Json = jsx:encode(Rows),
      Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req0),
      {ok, Req};
    {error, Reason} ->
      Json = jsx:encode(#{error => Reason}),
      Req = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, Json, Req0),
      {ok, Req}
  end.

update(Body, Req0) ->
  {struct, DecodeMap}= jsx:decode(Body),
  Name = maps:get(<<"Name">>, DecodeMap),
  Sql = "INSERT INTO sports(name) VALUES ($1) RETURNING id",
  Params = [binary_to_list(Name)],
  case linein_db:equery(Sql, Params) of
    {ok, _, [{Id}]} ->
      Json1 = jsx:encode(#{id => Id}),
      Req = cowboy_req:reply(201, #{<<"content-type">> => <<"application/json">>}, Json1, Req0),
      {ok, Req};
    {error, Reason} ->
      Json1 = jsx:encode(#{error => Reason}),
      Req = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, Json1, Req0),
      {ok, Req}
  end.