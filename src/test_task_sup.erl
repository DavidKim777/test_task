-module(test_task_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, DbPoolCfg} = application:get_env(test_task, db_pool),
	{ok, DbCfg} = application:get_env(test_task, db),
	{ok, RedisPoolCfg} = application:get_env(test_task, redis_pool),
	PgPoolChild = poolboy:child_spec(
		#{
			name => maps:get(name, DbPoolCfg),
			worker_module => maps:get(worker_module, DbPoolCfg),
			size => maps:get(size, DbPoolCfg),
			max_overflow => maps:get(max_overflow, DbPoolCfg)
		},
		[DbCfg]
	),
	RedisChild = {redis_pool,
		{redis_worker, start_link, [maps:get(host, RedisPoolCfg),
			maps:get(port, RedisPoolCfg)]},
		permanent, 5000, worker, [redis_worker]},
	{ok, {{one_for_one, 10, 10}, [PgPoolChild, RedisChild]}}.
