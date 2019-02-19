EXECUTABLES := docker

K := $(foreach exec,$(EXECUTABLES),\
        $(if $(shell which $(exec)),$(exec),$(error "No $(exec) in PATH")))

build:
	nix-build --attr webserver release.nix

image:
	nix-build --attr docker release.nix
	docker load -i result

run:
	docker run -it -e PORT=3000 -p 3000:3000 webserver:latest

.PHONY: docker-image
