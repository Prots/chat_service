PROJECT = chat_service

DEPS = lager ranch jsx

dep_lager  = git https://github.com/basho/lager.git             3.0.1
dep_ranch  = git https://github.com/ninenines/ranch.git         1.1.0
dep_jsx    = git https://github.com/talentdeficit/jsx.git       v2.4.0

ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'

TEST_DIR = test

ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)

include erlang.mk
