-module(test_task_handler).

-export([init/2]).

init(Req0, State) ->
  Method = cowboy_req:method(Req0),
  Path = cowboy_req:path(Req0),
  {ok, Body, _} = cowboy_req:read_body(Req0),
  {struct, Arg}= jsx:decode(Body),
  {Answer, Map} = dispatch(Method, Path, Arg),
  StatusCode = case Answer of
    ok ->
      200;
    error ->
      500
  end,
  Json = jsx:encode(Map),
  Headers = #{<<"content-type">> => <<"application/json">>},
  Req = cowboy_req:reply(StatusCode, Headers, Json, Req0),
  {ok, Req, State}.

dispatch(<<"POST">>, Path, Arg) ->
  case Path of
    <<"/sport/get_all">> ->
      {Answer, Map} = test_task_sport_api:get(Arg),
      {Answer, Map};
    <<"/sport/update">> ->
      {Answer, Map}= test_task_sport_api:update(Arg),
      {Answer, Map};
    <<"/sport/create">> ->
      {Answer, Map} = test_task_sport_api:create(Arg),
      {Answer, Map};
    <<"/sport/delete">> ->
      {Answer, Map} = test_task_sport_api:delete(Arg),
      {Answer, Map};
    <<"/category/get_all">> ->
      {Answer, Map} = test_task_category_api:get(Arg),
      {Answer, Map};
    <<"/category/create">> ->
      {Answer, Map} = test_task_category_api:create(Arg),
      {Answer, Map};
    <<"/category/update">> ->
      {Answer, Map} = test_task_category_api:update(Arg),
      {Answer, Map};
    <<"/category/delete">> ->
      {Answer, Map} = test_task_category_api:delete(Arg),
      {Answer, Map};
    <<"tournament/get_all">> ->
      {Answer, Map} = test_task_tournament_api:get(Arg),
      {Answer, Map};
    <<"tournament/create">> ->
      {Answer, Map} = test_task_tournament_api:create(Arg),
      {Answer, Map};
    <<"tournament/update">> ->
      {Answer, Map} = test_task_tournament_api:update(Arg),
      {Answer, Map};
    <<"tournament/delete">> ->
      {Answer, Map} = test_task_tournament_api:delete(Arg),
      {Answer, Map};
    <<"event/get_all">> ->
      {Answer, Map} = test_task_event_api:get(Arg),
      {Answer, Map};
    <<"event/create">> ->
      {Answer, Map} = test_task_event_api:create(Arg),
      {Answer, Map};
    <<"event/update">> ->
      {Answer, Map} = test_task_event_api:update(Arg),
      {Answer, Map};
    <<"event/delete">> ->
      {Answer, Map} = test_task_event_api:delete(Arg),
      {Answer, Map};
    <<"market/get_all">> ->
      {Answer, Map} = test_task_market_api:get(Arg),
      {Answer, Map};
    <<"marcet/create">> ->
      {Answer, Map} = test_task_market_api:create(Arg),
      {Answer, Map};
    <<"market/update">> ->
      {Answer, Map}= test_task_market_api:update(Arg),
      {Answer, Map};
    <<"market/delete">> ->
      {Answer, Map} = test_task_market_api:delete(Arg),
      {Answer, Map};
    <<"outcome/get_all">> ->
      {Answer, Map} = test_task_outcome_api:get(Arg),
      {Answer, Map};
    <<"outcome/create">> ->
      {Answer, Map} = test_task_outcome_api:create(Arg),
      {Answer, Map};
    <<"outcome/update">> ->
      {Answer, Map} = test_task_outcome_api:update(Arg),
      {Answer, Map};
    <<"outcome/delete">> ->
      {Answer, Map} = test_task_outcome_api:delete(Arg),
      {Answer, Map}
  end.