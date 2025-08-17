-module(test_task_handler).

-export([init/2]).

init(Req0, State) ->
  Method = cowboy_req:method(Req0),
  Path = cowboy_req:path(Req0),
  {ok, Body, _} = cowboy_req:read_body(Req0),
  Arg = test_task_protocol:decode(Body),
  {Reply, Arg1} = dispatch(Method, Path, Arg),
  Json = test_task_protocol:encode(Arg1),
  StatusCode = case Reply of
    ok ->
      200;
    error ->
      500
  end,
  Headers = #{<<"content-type">> => <<"application/json">>},
  Req = cowboy_req:reply(StatusCode, Headers, Json, Req0),
  {ok, Req, State}.

dispatch(<<"POST">>, Path, Arg) ->
   case Path of
    <<"/sport/get_all">> ->
       test_task_sport_api:get(Arg);
    <<"/sport/update">> ->
       test_task_sport_api:update(Arg);
    <<"/sport/create">> ->
      test_task_sport_api:create(Arg);
    <<"/sport/delete">> ->
      test_task_sport_api:delete(Arg);
    <<"/category/get_all">> ->
      test_task_category_api:get(Arg);
    <<"/category/create">> ->
      test_task_category_api:create(Arg);
    <<"/category/update">> ->
      test_task_category_api:update(Arg);
    <<"/category/delete">> ->
      test_task_category_api:delete(Arg);
    <<"tournament/get_all">> ->
      test_task_tournament_api:get(Arg);
    <<"tournament/create">> ->
      test_task_tournament_api:create(Arg);
    <<"tournament/update">> ->
      test_task_tournament_api:update(Arg);
    <<"tournament/delete">> ->
      test_task_tournament_api:delete(Arg);
    <<"event/get_all">> ->
      test_task_event_api:get(Arg);
    <<"event/create">> ->
      test_task_event_api:create(Arg);
    <<"event/update">> ->
      test_task_event_api:update(Arg);
    <<"event/delete">> ->
      test_task_event_api:delete(Arg);
    <<"market/get_all">> ->
      test_task_market_api:get(Arg);
    <<"marcet/create">> ->
      test_task_market_api:create(Arg);
    <<"market/update">> ->
      test_task_market_api:update(Arg);
    <<"market/delete">> ->
      test_task_market_api:delete(Arg);
    <<"outcome/get_all">> ->
      test_task_outcome_api:get(Arg);
    <<"outcome/create">> ->
      test_task_outcome_api:create(Arg);
    <<"outcome/update">> ->
      test_task_outcome_api:update(Arg);
    <<"outcome/delete">> ->
      test_task_outcome_api:delete(Arg)
   end.