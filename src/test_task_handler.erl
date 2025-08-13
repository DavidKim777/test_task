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
      {Reply, Map} = test_task_sport_api:get(Body),
      {Reply, Map};
    <<"/sport/update">> ->
      {Reply, Map} = test_task_sport_api:update(Body),
      {Reply, Map};
    <<"/sport/create">> ->
      {Reply, Map} = test_task_sport_api:create(Body),
      {Reply, Map};
    <<"/sport/delete">> ->
      {Reply, Map} = test_task_sport_api:delete(Body),
      {Reply, Map};
    <<"/category/get_all">> ->
      {Reply, Map} = test_task_category_api:get(Body),
      {Reply, Map};
    <<"/category/create">> ->
      {Reply, Map} = test_task_category_api:create(Body),
      {Reply, Map};
    <<"/category/update">> ->
      {Reply, Map} = test_task_category_api:update(Body),
      {Reply, Map};
    <<"/category/delete">> ->
      {Reply, Map} = test_task_category_api:delete(Body),
      {Reply, Map};
    <<"tournament/get_all">> ->
      {Reply, Map} = test_task_tournament_api:get(Body),
      {Reply, Map};
    <<"tournament/create">> ->
      {Reply, Map} = test_task_tournament_api:create(Body),
      {Reply, Map};
    <<"tournament/update">> ->
      {Reply, Map} = test_task_tournament_api:update(Body),
      {Reply, Map};
    <<"tournament/delete">> ->
      {Reply, Map} = test_task_tournament_api:delete(Body),
      {Reply, Map};
    <<"event/get_all">> ->
      {Reply, Map} = test_task_event_api:get(Body),
      {Reply, Map};
    <<"event/create">> ->
      {Reply, Map} = test_task_event_api:create(Body),
      {Reply, Map};
    <<"event/update">> ->
      {Reply, Map} = test_task_event_api:update(Body),
      {Reply, Map};
    <<"event/delete">> ->
      {Reply, Map} = test_task_event_api:delete(Body),
      {Reply, Map};
    <<"market/get_all">> ->
      {Reply, Map} = test_task_market_api:get(Body),
      {Reply, Map};
    <<"marcet/create">> ->
      {Reply, Map} = test_task_market_api:create(Body),
      {Reply, Map};
    <<"market/update">> ->
      {Reply, Map} = test_task_market_api:update(Body),
      {Reply, Map};
    <<"market/delete">> ->
      {Reply, Map} = test_task_market_api:delete(Body),
      {Reply, Map};
    <<"outcome/get_all">> ->
      {Reply, Map} = test_task_outcome_api:get(Body),
      {Reply, Map};
    <<"outcome/create">> ->
      {Reply, Map} = test_task_outcome_api:create(Body),
      {Reply, Map};
    <<"outcome/update">> ->
      {Reply, Map} = test_task_outcome_api:update(Body),
      {Reply, Map};
    <<"outcome/delete">> ->
      {Reply, Map} = test_task_outcome_api:delete(Body),
      {Reply, Map}
  end.