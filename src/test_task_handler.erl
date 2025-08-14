-module(test_task_handler).

-export([init/2]).

init(Req0, State) ->
  Method = cowboy_req:method(Req0),
  Path = cowboy_req:path(Req0),
  {ok, Body, _} = cowboy_req:read_body(Req0),
  {Reply, Json} = dispatch(Method, Path, Body),
  StatusCode = case Reply of
    ok ->
      200;
    error ->
      500
  end,
  Headers = #{<<"content-type">> => <<"application/json">>},
  Req = cowboy_req:reply(StatusCode, Headers, Json, Req0),
  {ok, Req, State}.

dispatch(<<"POST">>, Path, Body) ->
  case Path of
    <<"/sport/get_all">> ->
      {Reply, Json} = test_task_sport_api:get(Body),
      {Reply, Json};
    <<"/sport/update">> ->
      {Reply, Json} = test_task_sport_api:update(Body),
      {Reply, Json};
    <<"/sport/create">> ->
      {Reply, Json} = test_task_sport_api:create(Body),
      {Reply, Json};
    <<"/sport/delete">> ->
      {Reply, Json} = test_task_sport_api:delete(Body),
      {Reply, Json};
    <<"/category/get_all">> ->
      {Reply, Json} = test_task_category_api:get(Body),
      {Reply, Json};
    <<"/category/create">> ->
      {Reply, Json} = test_task_category_api:create(Body),
      {Reply, Json};
    <<"/category/update">> ->
      {Reply, Json} = test_task_category_api:update(Body),
      {Reply, Json};
    <<"/category/delete">> ->
      {Reply, Json} = test_task_category_api:delete(Body),
      {Reply, Json};
    <<"tournament/get_all">> ->
      {Reply, Json} = test_task_tournament_api:get(Body),
      {Reply, Json};
    <<"tournament/create">> ->
      {Reply, Json} = test_task_tournament_api:create(Body),
      {Reply, Json};
    <<"tournament/update">> ->
      {Reply, Json} = test_task_tournament_api:update(Body),
      {Reply, Json};
    <<"tournament/delete">> ->
      {Reply, Json} = test_task_tournament_api:delete(Body),
      {Reply, Json};
    <<"event/get_all">> ->
      {Reply, Json} = test_task_event_api:get(Body),
      {Reply, Json};
    <<"event/create">> ->
      {Reply, Json} = test_task_event_api:create(Body),
      {Reply, Json};
    <<"event/update">> ->
      {Reply, Json} = test_task_event_api:update(Body),
      {Reply, Json};
    <<"event/delete">> ->
      {Reply, Json} = test_task_event_api:delete(Body),
      {Reply, Json};
    <<"market/get_all">> ->
      {Reply, Json} = test_task_market_api:get(Body),
      {Reply, Json};
    <<"marcet/create">> ->
      {Reply, Json} = test_task_market_api:create(Body),
      {Reply, Json};
    <<"market/update">> ->
      {Reply, Json} = test_task_market_api:update(Body),
      {Reply, Json};
    <<"market/delete">> ->
      {Reply, Json} = test_task_market_api:delete(Body),
      {Reply, Json};
    <<"outcome/get_all">> ->
      {Reply, Json} = test_task_outcome_api:get(Body),
      {Reply, Json};
    <<"outcome/create">> ->
      {Reply, Json} = test_task_outcome_api:create(Body),
      {Reply, Json};
    <<"outcome/update">> ->
      {Reply, Json} = test_task_outcome_api:update(Body),
      {Reply, Json};
    <<"outcome/delete">> ->
      {Reply, Json} = test_task_outcome_api:delete(Body),
      {Reply, Json}
  end.