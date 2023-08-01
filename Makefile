.PHONY: test compile clean dialyzer bootstrap escript

all: compile

compile:
	@rebar3 compile

test:
	@rebar3 eunit

clean:
	@rebar3 clean

neotoma.plt:
	@rebar3 dialyzer -- --build_plt --apps erts kernel stdlib compiler crypto hipe \
		syntax_tools --output_plt neotoma.plt

dialyzer: compile neotoma.plt
	@rebar3 dialyzer -- --plt neotoma.plt ebin

xref: compile
	@rebar3 xref skip_deps=true

bootstrap: compile
	@erl -pz ebin -b start_sasl -noshell -s init stop -s neotoma bootstrap
	@rebar3 compile

escript:
	@rebar3 escriptize
