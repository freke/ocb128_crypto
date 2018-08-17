PROJECT=ocb128_crypto
DOCKER_IMG= freke/ocb128_crypto
DOCKER=docker-compose -f docker-compose.yml run --rm --service-ports $(PROJECT)
REBAR=$(DOCKER) rebar3

.PHONY: all compile test clean doc

all: compile test

compile:
	$(REBAR) compile

test: dialyzer xref eunit ct ctest
	$(REBAR) cover

ctest: build_docker
	$(info ************  Tets C Code ************)
	docker run --rm -v $(shell pwd):/src/ocb128_crypto -w /src/ocb128_crypto ${DOCKER_IMG} gcc test/ocb128_test.c c_src/crypt.c -Ic_src -lcrypto -o ocb128Test
	docker run --rm -v $(shell pwd):/src/ocb128_crypto -w /src/ocb128_crypto ${DOCKER_IMG} ./ocb128Test
	rm -f ocb128Test

eunit:
	$(REBAR) eunit

ct:
	$(REBAR) ct

dialyzer:
	$(REBAR) dialyzer

xref: build_docker
	$(REBAR) xref

release:
	$(DOCKER) rm -Rf _build/prod
	$(REBAR) as prod compile
	$(REBAR) as prod release
	docker build --pull=true --no-cache=true --force-rm=true -t freke/$(PROJECT):0.0.1 -t freke/$(PROJECT):latest -f docker/Dockerfile_prod .

doc:
	$(REBAR) edoc

clean: build_docker
	$(REBAR) clean --all
	$(DOCKER) rm -Rf _build/test/cover
	$(DOCKER) rm -Rf _build/test/logs

distclean: build_docker clean
	$(DOCKER) rm -Rf _build
	$(DOCKER) rm -Rf docs

upgrade: build_docker
	$(REBAR) upgrade

shell: build_docker
	$(REBAR) shell

build_docker:
	docker build -t ${DOCKER_IMG} docker
