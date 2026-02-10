CONTAINER_IMAGE = iocanel/emacs

all: container-build container-test
UID := $(shell id -u)
GID := $(shell id -g)
HOME_DIR := $(HOME)
DOCKER_RUN = docker run --rm -it --net=host -v $(HOME_DIR):$(HOME_DIR) -v /tmp/.X11-unix:/tmp/.X11-unix -e DISPLAY=$(DISPLAY) --user $(UID):$(GID) -e HOME=$(HOME_DIR) -w $(HOME_DIR) --tmpfs /tmp/emacs-state -e XDG_STATE_HOME=/tmp/emacs-state $(CONTAINER_IMAGE)

container-build:
	docker build -f Dockerfile -t $(CONTAINER_IMAGE) .

container-rebuild:
	docker build --no-cache -f Dockerfile -t $(CONTAINER_IMAGE) .

container-shell: container-build
	$(DOCKER_RUN)

container-debug: container-build
	docker run --rm -it -v $(PWD):/workspace -v $(HOME_DIR):$(HOME_DIR) -e TERM=xterm-256color -e HOME=$(HOME_DIR) --user $(UID):$(GID) $(CONTAINER_IMAGE) emacs -nw --debug-init --eval "(toggle-debug-on-error)"

container-test-java: container-build
	docker run --rm --net=host -v $(HOME_DIR):$(HOME_DIR) -v /tmp/.X11-unix:/tmp/.X11-unix -e DISPLAY=$(DISPLAY) --user $(UID):$(GID) -e HOME=$(HOME_DIR) -w $(HOME_DIR) --tmpfs /tmp/emacs-state -e XDG_STATE_HOME=/tmp/emacs-state -v $(PWD)/tests/java/mode.el:/tmp/test-mode.el $(CONTAINER_IMAGE) emacs --batch --load /tmp/test-mode.el

container-test-go: container-build
	docker run --rm --net=host -v $(HOME_DIR):$(HOME_DIR) -v /tmp/.X11-unix:/tmp/.X11-unix -e DISPLAY=$(DISPLAY) --user $(UID):$(GID) -e HOME=$(HOME_DIR) -w $(HOME_DIR) --tmpfs /tmp/emacs-state -e XDG_STATE_HOME=/tmp/emacs-state -v $(PWD)/tests/go/mode.el:/tmp/test-mode.el $(CONTAINER_IMAGE) emacs --batch --load /tmp/test-mode.el

container-test-python: container-build
	docker run --rm --net=host -v $(HOME_DIR):$(HOME_DIR) -v /tmp/.X11-unix:/tmp/.X11-unix -e DISPLAY=$(DISPLAY) --user $(UID):$(GID) -e HOME=$(HOME_DIR) -w $(HOME_DIR) --tmpfs /tmp/emacs-state -e XDG_STATE_HOME=/tmp/emacs-state -v $(PWD)/tests/python/mode.el:/tmp/test-mode.el $(CONTAINER_IMAGE) emacs --batch --load /tmp/test-mode.el

container-test-javascript: container-build
	docker run --rm --net=host -v $(HOME_DIR):$(HOME_DIR) -v /tmp/.X11-unix:/tmp/.X11-unix -e DISPLAY=$(DISPLAY) --user $(UID):$(GID) -e HOME=$(HOME_DIR) -w $(HOME_DIR) --tmpfs /tmp/emacs-state -e XDG_STATE_HOME=/tmp/emacs-state -v $(PWD)/tests/javascript/mode.el:/tmp/test-mode.el $(CONTAINER_IMAGE) emacs --batch --load /tmp/test-mode.el

container-test-typescript: container-build
	docker run --rm --net=host -v $(HOME_DIR):$(HOME_DIR) -v /tmp/.X11-unix:/tmp/.X11-unix -e DISPLAY=$(DISPLAY) --user $(UID):$(GID) -e HOME=$(HOME_DIR) -w $(HOME_DIR) --tmpfs /tmp/emacs-state -e XDG_STATE_HOME=/tmp/emacs-state -v $(PWD)/tests/typescript/mode.el:/tmp/test-mode.el $(CONTAINER_IMAGE) emacs --batch --load /tmp/test-mode.el

container-test-rust: container-build
	docker run --rm --net=host -v $(HOME_DIR):$(HOME_DIR) -v /tmp/.X11-unix:/tmp/.X11-unix -e DISPLAY=$(DISPLAY) --user $(UID):$(GID) -e HOME=$(HOME_DIR) -w $(HOME_DIR) --tmpfs /tmp/emacs-state -e XDG_STATE_HOME=/tmp/emacs-state -v $(PWD)/tests/rust/mode.el:/tmp/test-mode.el $(CONTAINER_IMAGE) emacs --batch --load /tmp/test-mode.el

container-test-c: container-build
	docker run --rm --net=host -v $(HOME_DIR):$(HOME_DIR) -v /tmp/.X11-unix:/tmp/.X11-unix -e DISPLAY=$(DISPLAY) --user $(UID):$(GID) -e HOME=$(HOME_DIR) -w $(HOME_DIR) --tmpfs /tmp/emacs-state -e XDG_STATE_HOME=/tmp/emacs-state -v $(PWD)/tests/c/mode.el:/tmp/test-mode.el $(CONTAINER_IMAGE) emacs --batch --load /tmp/test-mode.el

container-test: container-test-java container-test-go container-test-python container-test-javascript container-test-typescript container-test-rust container-test-c
