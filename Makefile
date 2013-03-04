PROJECT = mechanus_fsm

# Options ##############################################################
EUNIT_OPTS = [verbose]
ERLC_OPTS = +debug_info +nowarn_shadow_vars +warnings_as_errors

# Dependecies ##########################################################
DEPS = stdlib2 eon

dep_stdlib2 = git git://github.com/kivra/stdlib2.git master
dep_eon     = git git://github.com/kivra/eon.git     master

# Standard targets #####################################################
include erlang.mk

# Utilities ############################################################
.PHONY: repl eunit_repl

repl:
	erl -pa ebin deps/*/ebin

eunit_repl:
	erl -pa .eunit deps/*/ebin

# eof
