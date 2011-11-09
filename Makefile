
.PHONEY: rel

all: deps compile

compile: deps
	./rebar compile
	./rebar escriptize

deps:
	./rebar get-deps

clean:
	./rebar clean
test: compile
	./rebar eunit app=ibeam
	./rebar ct

distclean: clean relclean
	./rebar delete-deps

rel: deps compile
	./rebar escriptize

relclean:
	rm -fr ibeam


