RELNAME=herp

all:
	rebar compile; relx

run:
	_rel/bin/$(RELNAME) console

clean:
	rm -r _rel ebin/ deps

deps:
	rebar get-deps

doc:
	erl -noshell \
		-eval 'edoc:application($(RELNAME), ".", []), init:stop().'
