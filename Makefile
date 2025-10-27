CONTAINER_IMAGE = iocanel/emacs
UID := $(shell id -u)
GID := $(shell id -g)
HOME_DIR := $(HOME)
DOCKER_RUN = docker run --rm -it --net=host -v $(HOME_DIR):$(HOME_DIR) -v /tmp/.X11-unix:/tmp/.X11-unix -e DISPLAY=$(DISPLAY) --user $(UID):$(GID) -e HOME=$(HOME_DIR) -w $(HOME_DIR) --tmpfs /tmp/emacs-state -e XDG_STATE_HOME=/tmp/emacs-state $(CONTAINER_IMAGE)

container-build:
	docker build -f Dockerfile -t $(CONTAINER_IMAGE) .

container-shell: container-build
	$(DOCKER_RUN)
