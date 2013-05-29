RELNAME=herp

all:
	rebar compile

run:
	_rel/bin/$(RELNAME) console
