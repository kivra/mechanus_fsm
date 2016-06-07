PROJECT = mechanus_fsm

# Options ##############################################################
EUNIT_OPTS = [verbose]
ERLC_OPTS = +debug_info +nowarn_shadow_vars +warnings_as_errors \
            -DS2_USE_LAGER

# Dependecies ##########################################################
DEPS = stdlib2 eon lager

dep_lager   = git git://github.com/kivra/lager       master
dep_stdlib2 = git git://github.com/kivra/stdlib2.git master
dep_eon     = git git://github.com/kivra/eon.git     master

# Standard targets #####################################################
include erlang.mk

# Utilities ############################################################
.PHONY: repl eunit_repl

repl: app
	@exec erl -pa $(PWD)/ebin -pa $(PWD)/deps/**/ebin \
            -pa $(PWD)/deps/**/deps/**/ebin

eunit_repl:
	erl -pa .eunit deps/*/ebin

# eof
