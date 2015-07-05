.PHONY: deps eqc-compile test compile clean dialyzer bootstrap escript
REBAR ?= $(shell which rebar3)

all: compile

compile:
	@${REBAR} compile

test:
	@${REBAR} eunit

clean:
	@${REBAR} clean

dialyzer:
	@${REBAR} dialyzer

bootstrap: compile
	@erl -pz ebin -b start_sasl -noshell -s init stop -s neotoma bootstrap
	@${REBAR} compile

escript:
	@${REBAR} escriptize

eqc-compile:
	-mkdir ebin
	erl -make
