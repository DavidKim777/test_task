-module(test_task_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [{"/", test_task_handler, []}]}
	]),
	{ok, _} = cowboy:start_clear(test_task_http_listener,
		[{port, 8080}],
		#{env => #{dispatch => Dispatch}}
	),
	test_task_sup:start_link().

stop(_State) ->
	ok = cowboy:stop_listener(test_task_listener).

