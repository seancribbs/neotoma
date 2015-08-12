.PHONY: deps eqc-compile test compile clean dialyzer bootstrap escript
REBAR3_URL=https://s3.amazonaws.com/rebar3/rebar3

# If there is a rebar in the current directory, use it
ifeq ($(wildcard rebar3),rebar3)
REBAR3 = $(CURDIR)/rebar3
endif

# Fallback to rebar on PATH
REBAR3 ?= $(shell which rebar3)

# And finally, prep to download rebar if all else fails
ifeq ($(REBAR3),)
REBAR3 = $(CURDIR)/rebar3
endif

all: compile

$(REBAR3):
	curl -Lo rebar3 $(REBAR3_URL) || wget $(REBAR3_URL)
	chmod a+x rebar3

compile: $(REBAR3)
	@${REBAR3} compile

test: $(REBAR3)
	@${REBAR3} eunit

clean: $(REBAR3)
	@${REBAR3} clean

dialyzer: $(REBAR3)
	@${REBAR3} dialyzer

bootstrap: compile
	@erl -pz ebin -b start_sasl -noshell -s init stop -s neotoma bootstrap
	@${REBAR3} compile

escript: $(REBAR3)
	@${REBAR3} escriptize

eqc-compile: $(REBAR3)
	@TERM=dumb ${REBAR3} as quickcheck_ci compile

