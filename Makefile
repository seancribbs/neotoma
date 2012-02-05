all: compile

compile:
	@ rebar compile

tests:
	@ rebar eunit

clean:
	@ rebar clean

dialyze: compile
	@ rebar dialyze

bootstrap: compile
	@ erl -pz ebin -b start_sasl -noshell -s init stop -eval 'neotoma:bootstrap().'
	@ rebar compile

escript:
	@ rebar escriptize
