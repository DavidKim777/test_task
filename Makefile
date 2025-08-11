PROJECT = test_task
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0
DEPS = cowboy epgsql jsx eredis liver lager poolboy

dep_cowboy_commit = 2.13.0
dep_epgsql = git https://github.com/epgsql/epgsql 4.6.0
dep_jsx = git https://github.com/talentdeficit/jsx v3.1.0
dep_eredis = git https://github.com/wooga/eredis v1.2.0
dep_liver = git https://github.com/erlangbureau/liver 0.9.0
dep_lager = git https://github.com/erlang-lager/lager 3.9.2
dep_poolboy = git https://github.com/devinus/poolboy 1.5.2

CONFIG = config/test_task.config
RUN_ERL = erl -pa ebin -pa deps/*/ebin -config $(CONFIG) -s $(PROJECT)

include erlang.mk

run: $(RUN_ERL)
