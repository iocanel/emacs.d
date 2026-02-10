CONTAINER_IMAGE = iocanel/emacs

all: container-build container-test

test: test-java test-go test-python test-javascript test-typescript test-rust test-c

clean:
	@echo "Cleaning test artifacts..."
	@find tests -name "*.elc" -type f -delete 2>/dev/null || true
	@echo "✅ Test artifacts cleaned"

help:
	@echo "Emacs Configuration - Available Targets"
	@echo "========================================"
	@echo ""
	@echo "Local Tests:"
	@echo "  test                  - Run all local tests"
	@echo "  test-java             - Test Java mode availability"
	@echo "  test-go               - Test Go mode availability"
	@echo "  test-python           - Test Python mode availability"
	@echo "  test-javascript       - Test JavaScript mode availability"
	@echo "  test-typescript       - Test TypeScript mode availability"
	@echo "  test-rust             - Test Rust mode availability"
	@echo "  test-c                - Test C mode availability"
	@echo ""
	@echo "Container Testing:"
	@echo "  container-build       - Build container image"
	@echo "  container-rebuild     - Build container image (no cache)"
	@echo "  container-test        - Run all tests in container"
	@echo "  container-shell       - Interactive shell in container"
	@echo "  container-debug       - Debug Emacs init in container"
	@echo ""
	@echo "Utilities:"
	@echo "  clean                 - Clean test artifacts"
	@echo "  help                  - Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make test             # Run all local tests"
	@echo "  make container-test   # Run all tests in container"
	@echo "  make test-java        # Test only Java mode"

test-java:
	emacs --batch --load tests/java/mode.el

test-go:
	emacs --batch --load tests/go/mode.el

test-python:
	emacs --batch --load tests/python/mode.el

test-javascript:
	emacs --batch --load tests/javascript/mode.el

test-typescript:
	emacs --batch --load tests/typescript/mode.el

test-rust:
	emacs --batch --load tests/rust/mode.el

test-c:
	emacs --batch --load tests/c/mode.el
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

.PHONY: all test clean help container-build container-rebuild container-shell container-debug container-test
