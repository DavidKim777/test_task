PROJECT = test_task
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0
DEPS = cowboy epgsql jsx eredis liver lager

dep_cowboy_commit = 2.13.0
dep_epgsql = git https://github.com/epgsql/epgsql 4.6.0
dep_jsx = git https://github.com/talentdeficit/jsx v3.1.0
dep_eredis = git https://github.com/wooga/eredis v1.2.0
dep_liver = git https://github.com/erlangbureau/liver 0.9.0
dep_lager = git https://github.com/erlang-lager/lager 3.9.2

include erlang.mk
