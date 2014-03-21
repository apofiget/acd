REBAR = ./rebar

compile:
	@$(REBAR) get-deps
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	@rm -f erl_crash.dump

node:
	@$(REBAR) generate

doc:
	cd apps/acd
	@$(REBAR) doc
