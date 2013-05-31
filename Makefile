RELNAME=herp

all:
	rebar compile; relx

run:
	_rel/bin/$(RELNAME) console
