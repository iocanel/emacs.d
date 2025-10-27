CONTAINER_IMAGE = iocanel/emacs
UID := $(shell id -u)
GID := $(shell id -g)
HOME_DIR := $(HOME)
DOCKER_RUN = docker run --rm -it --net=host -v $(HOME_DIR):$(HOME_DIR) -v /tmp/.X11-unix:/tmp/.X11-unix -e DISPLAY=$(DISPLAY) --user $(UID):$(GID) -e HOME=$(HOME_DIR) -w $(HOME_DIR) --tmpfs /tmp/emacs-state -e XDG_STATE_HOME=/tmp/emacs-state $(CONTAINER_IMAGE)

container-build:
	docker build -f Dockerfile -t $(CONTAINER_IMAGE) .

container-shell: container-build
	$(DOCKER_RUN)

container-test-java: container-build
	docker run --rm -v $(PWD)/tests/java/mode.el:/emacs/.config/emacs/mode.el $(CONTAINER_IMAGE) emacs --batch --load /emacs/.config/emacs/mode.el

container-test-go: container-build
	docker run --rm -v $(PWD)/tests/go/mode.el:/emacs/.config/emacs/mode.el $(CONTAINER_IMAGE) emacs --batch --load /emacs/.config/emacs/mode.el

container-test-python: container-build
	docker run --rm -v $(PWD)/tests/python/mode.el:/emacs/.config/emacs/mode.el $(CONTAINER_IMAGE) emacs --batch --load /emacs/.config/emacs/mode.el

container-test-javascript: container-build
	docker run --rm -v $(PWD)/tests/javascript/mode.el:/emacs/.config/emacs/mode.el $(CONTAINER_IMAGE) emacs --batch --load /emacs/.config/emacs/mode.el

container-test-typescript: container-build
	docker run --rm -v $(PWD)/tests/typescript/mode.el:/emacs/.config/emacs/mode.el $(CONTAINER_IMAGE) emacs --batch --load /emacs/.config/emacs/mode.el

container-test-rust: container-build
	docker run --rm -v $(PWD)/tests/rust/mode.el:/emacs/.config/emacs/mode.el $(CONTAINER_IMAGE) emacs --batch --load /emacs/.config/emacs/mode.el

container-test-c: container-build
	docker run --rm -v $(PWD)/tests/c/mode.el:/emacs/.config/emacs/mode.el $(CONTAINER_IMAGE) emacs --batch --load /emacs/.config/emacs/mode.el

container-test: container-test-java container-test-go container-test-python container-test-javascript container-test-typescript container-test-rust container-test-c
