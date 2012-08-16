.PHONY: deps xref

all: ecrepo test

ecrepo: compile
	@rebar skip_deps=true escriptize

compile: deps
	@rebar compile

test: compile
	@rebar skip_deps=true eunit

erl:
	erl -pa ebin deps/*/ebin

clean:
	@rebar clean

deps:
	@rebar get-deps

xref: compile
	@rebar skip_deps=true xref
