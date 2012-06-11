
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

# Expect to find rebar in the PATH. If you don't have rebar, you can get it
# from https://github.com/basho/rebar .
REBAR=./rebar

.PHONY: all test

all: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

test: all
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps
