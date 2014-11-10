REBAR = `which rebar`
RELX = `which relx`

all: deps compile

deps:
	@( $(REBAR) get-deps )

compile: clean
	@( $(REBAR) compile )

clean:
	@( $(REBAR) clean )

run:
	@( erl -pa  ebin deps/*/ebin -s privatepaste )

release: compile
	@( $(RELX) release )

.PHONY: all deps compile clean run
