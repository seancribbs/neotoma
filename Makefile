.PHONY: deps eqc-ci test compile clean dialyzer bootstrap escript

all: deps compile

deps:
	@./rebar get-deps

compile:
	@./rebar compile

test:
	@./rebar eunit

clean:
	@./rebar clean

neotoma.plt:
	@dialyzer --build_plt --apps erts kernel stdlib compiler crypto hipe \
		syntax_tools --output_plt neotoma.plt

dialyzer: compile neotoma.plt
	@dialyzer --plt neotoma.plt ebin

bootstrap: compile
	@erl -pz ebin -b start_sasl -noshell -s init stop -s neotoma bootstrap
	@./rebar compile

escript:
	@./rebar escriptize

eqc-ci: clean all
	@./rebar -D EQC_CI -C rebar_eqc_ci.config qc compile_only=true --verbose
