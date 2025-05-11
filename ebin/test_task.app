{application, 'test_task', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['test_task_app','test_task_sup']},
	{registered, [test_task_sup]},
	{applications, [kernel,stdlib]},
	{optional_applications, []},
	{mod, {'test_task_app', []}},
	{env, []}
]}.