{application, 'test_task', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['test_task_app','test_task_db','test_task_sport_api','test_task_sport_handler','test_task_sup']},
	{registered, [test_task_sup]},
	{applications, [kernel,stdlib,cowboy,epgsql,jsx,eredis,liver,lager]},
	{optional_applications, []},
	{mod, {'test_task_app', []}},
	{env, []}
]}.