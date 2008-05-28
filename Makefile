APP_NAME=xmpp_rubyerl
VSN=0.1
APP_DEPS="sasl"

ERL_FILES		?=$(wildcard src/*.erl)
HRL_FILES		?=$(wildcard include/*.hrl)
MODULES			?=$(subst src/, ,         $(subst .erl,,${ERL_FILES}))
BEAM_FILES		?=$(subst src/, ebin/,    $(subst .erl,.beam,${ERL_FILES}))
INC_DIRS		?=$(subst lib/, -I lib/,  $(wildcard lib/*/include))
A_EBIN_DIRS		?=$(subst lib/, -pa lib/, $(wildcard lib/*/ebin))
Z_EBIN_DIRS		?=$(subst lib/, -pz lib/, $(wildcard lib/*/ebin))

INCLUDE			?=-I ${ERLANG_ROOT}/lib/stdlib-*/include ${D_INC_DIRS} -I ${EUNIT_ROOT}/include -I include
CODEPATH		?=${Z_EBIN_DIRS} -pz ebin

ERLC_CODEPATH		?=${A_EBIN_DIRS} -pz ${EUNIT_ROOT}/ebin -pz ebin
ERLC_FLAGS		?=+debug_info -W -o ebin

EXTRA_DIALYZER_BEAM_FILES ?=$(wildcard lib/oserl*/ebin/*.beam lib/common_lib*/ebin/*.beam lib/proc_reg*/ebin/*.beam)

NODE			?="-name ${APP_NAME}@127.0.0.1"

all: ${BEAM_FILES}

release: ${BEAM_FILES} test dialyzer.report docs

.PHONY: info clean docs test shell dialyzer.report shell_args

info:
	@echo Erlang root: +${ERLANG_ROOT}+
	@echo Eunit root: +${EUNIT_ROOT}+
	@echo Beam dirs: ${A_EBIN_DIRS}
	@echo Extra dialyzer beam files: ${EXTRA_DIALYZER_BEAM_FILES}

clean:
	rm -f ebin/*.beam priv/sasl/* priv/sasl.log

ebin/%.beam: src/%.erl ${HRL_FILES}
	@echo "$@: "
	erlc ${ERLC_FLAGS} ${ERLC_CODEPATH} ${INCLUDE} $<

docs: ${ERL_FILES}
	erl -noshell -run edoc_run application "'$(APP_NAME)'" '"."' '[{def,{vsn,"$(VSN)"}}]'

test: ${BEAM_FILES}
	erl $(CODEPATH) -config priv/${APP_NAME} -eval "lists:map(fun(A) -> {A,application:start(A)} end, [${APP_DEPS}]), application:load(${APP_NAME}), lists:foreach(fun (M) -> io:fwrite(\"Testing ~p:~n\", [M]), eunit:test(M) end, [`perl -e 'print join(",", qw(${MODULES}));'`])." -s init stop -noshell

shell_args:
	@(echo -ne "rr(\"include/*\").\nlists:map(fun(A) -> {A,application:start(A)} end, [${APP_DEPS}]).\napplication:start(${APP_NAME})." | pbcopy)

shell: ${BEAM_FILES}
	erl +K true -smp +A 10 ${NODE} -s ${APP_NAME} start -config priv/${APP_NAME} $(CODEPATH)

dialyzer.report: ${BEAM_FILES}
	@(dialyzer --verbose --succ_typings ${INCLUDE} ${A_EBIN_DIRS} -c ${BEAM_FILES} ${EXTRA_DIALYZER_BEAM_FILES}; if [ $$? != 1 ]; then true; else false; fi)
