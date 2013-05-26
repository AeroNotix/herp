all:
	rebar compile; cd src; erlc *.erl
	mv src/*.beam ebin/

run:
	erl -pa deps/*/ebin -pa ebin -s ssl -s inets
