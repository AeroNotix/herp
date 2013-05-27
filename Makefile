all:
	rebar compile

run:
	erl -pa deps/*/ebin -pa ebin/ -s ssl -s inets -s herp
