.PHONY: test compile clean dialyzer bootstrap escript

all: compile

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

xref: compile
	@./rebar xref skip_deps=true

bootstrap: compile
	@erl -pz ebin -b start_sasl -noshell -s init stop -s neotoma bootstrap
	@./rebar compile

escript:
	@./rebar escriptize
