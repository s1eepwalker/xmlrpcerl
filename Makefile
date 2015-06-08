export PATH:=/opt/entensys/otp/bin:$(PATH)

all:
	./rebar3 as dev compile
dev:
	./rebar3 as dev release
rel:
	./rebar3 as prod release
tar:
	./rebar3 as prod tar
clean:
	./rebar3 clean
	rm -rf ./_build
run: dev
	cd ./_build/dev/rel/xmlrpcerl/bin && ./xmlrpcerl console
update:
	./rebar3 update
