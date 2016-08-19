PROJECT=ocb128_crypto
DOCKER=docker-compose -f docker-compose.yml run --rm --service-ports $(PROJECT)
REBAR=$(DOCKER) rebar3

.PHONY: all compile test clean doc

all: compile test

compile:
	$(REBAR) compile

test: dialyzer xref eunit ct ctest
	$(REBAR) cover

ctest:
	$(info ************  Tets C Code ************)
	$(DOCKER) gcc test/ocb128_test.c c_src/crypt.c -Ic_src -lcrypto -o ocb128Test
	docker run --rm -v $(shell pwd):/src/ocb128_crypto ocb128crypto_ocb128_crypto /src/ocb128_crypto/ocb128Test
	rm -f ocb128Test

eunit:
	$(REBAR) eunit

ct:
	$(REBAR) ct

dialyzer:
	$(REBAR) dialyzer

xref:
	$(REBAR) xref

release:
	$(DOCKER) rm -Rf _build/prod
	$(REBAR) as prod compile
	$(REBAR) as prod release
	docker build --pull=true --no-cache=true --force-rm=true -t freke/$(PROJECT):0.0.1 -t freke/$(PROJECT):latest -f docker/Dockerfile_prod .

doc:
	$(REBAR) edoc

clean:
	$(REBAR) clean --all
	$(DOCKER) rm -Rf _build/test/cover
	$(DOCKER) rm -Rf _build/test/logs

distclean: clean
	$(DOCKER) rm -Rf _build
	$(DOCKER) rm -Rf docs

upgrade:
	$(REBAR) upgrade

shell:
	$(REBAR) shell
