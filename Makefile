BIN = mdbook
GEN := $(shell which $(BIN) 2> /dev/null)
DOWNLOAD = https://github.com/rust-lang/mdBook/releases
PUBLISH_DIR = book
PUBLISH_BRANCH = master
BUILDER_BRANCH = builder
TMP_GIT_DIR = /tmp/lfe-sicp-git

define BINARY_ERROR

No $(BIN) found in Path.

Download $(BIN) from $(DOWNLOAD).

endef

build:
ifndef GEN
	$(error $(BINARY_ERROR))
endif
	$(MAKE) backup-book-git
	@$(GEN) build
	$(MAKE) restore-book-git

serve:
	$(MAKE) backup-book-git
	@$(GEN) serve
	$(MAKE) restore-book-git

run: serve

clean:
	@rm -f $(PUBLISH_DIR)/README.md

book-init:
	@git submodule update --init --recursive

backup-book-git:
	@mkdir -p $(TMP_GIT_DIR)/
	@mv -v $(PUBLISH_DIR)/.git $(TMP_GIT_DIR)/

restore-book-git:
	@mv -v $(TMP_GIT_DIR)/.git $(PUBLISH_DIR)/

$(PUBLISH_DIR)/README.md:
	@echo '# Content for SICP, the LFE Edition' > $(PUBLISH_DIR)/README.md
	@echo 'Published at [lfe.io/books/sicp/](https://lfe.io/books/sicp/)' >> $(PUBLISH_DIR)/README.md
	@cd $(PUBLISH_DIR) && git add README.md

publish: clean build $(PUBLISH_DIR)/README.md
	-@cd $(PUBLISH_DIR) && \
	git add * && \
	git commit -am "Regenerated book content." > /dev/null && \
	git push origin $(PUBLISH_BRANCH) && \
	cd -  && \
	git add $(PUBLISH_DIR) && \
	git commit -am "Updated submodule for recently generated book content." && \
	git submodule update && \
	git push origin $(BUILDER_BRANCH)

build-publish: build publish
