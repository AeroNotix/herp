RELNAME=herp

all:
	rebar compile; relx

run:
	_rel/bin/$(RELNAME) console

clean:
	rm -r _rel ebin/ deps

deps:
	rebar get-deps
