-module(test_task_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	PoolboyConfig = [
		{name, {local, pg_pool}},
		{worker_module, test_task_db},
		{size, 5},
		{max_overflow, 2}
	],
	ChildSpec = [{
			db_pool, {poolboy, start_link, [PoolboyConfig]},
			prmanent, 5000,
			worker, [test_task_db]
	}],
	{ok, {{one_for_one, 10, 10}, ChildSpec}}.
