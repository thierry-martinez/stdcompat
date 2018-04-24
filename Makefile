.PHONY: dev test doc repl clean
dev:
	jbuilder build --dev

test:
	jbuilder runtest --dev

doc:
	jbuilder build @doc

repl:
	jbuilder utop .

clean:
	jbuilder clean
