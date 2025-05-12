-module(test_task_sport_handler).

-export([init/2]).

init(Req0, State) ->
  Method = cowboy_req:method(Req0),
  Path = cowboy_req:path(Req0),
  {ok, Body, _} = cowboy_req:read_body(Req0),
  {ok, Req} = dispatch(Method, Path, Body, Req0),
  {ok, Req, State}.

dispatch(<<"POST">>, <<"/sport/get_all">>, Body, Req0) ->
  test_task_api:get(Body, Req0);
dispatch(<<"POST">>, <<"/sport/update">>, Body, Req0) ->
  test_task_api:update(Body, Req0).
