-module(test_task_protocol).

-export([encode/1, decode/1]).

decode(Json) ->
   jsx:decode(Json).

encode(Data) ->
   jsx:encode(Data).

