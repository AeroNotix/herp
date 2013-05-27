all:
	rebar compile; cd src; erlc *.erl
