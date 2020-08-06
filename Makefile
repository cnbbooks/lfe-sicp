BIN = mdbook
GEN := $(shell which $(BIN) 2> /dev/null)
DOWNLOAD = https://github.com/rust-lang/mdBook/releases
PUBLISH_DIR = book
PUBLISH_CONTENT = $(PUBLISH_DIR)/current
PUBLISH_BRANCH = master
BUILDER_BRANCH = builder

default: build

define BINARY_ERROR

No $(BIN) found in Path.

Download $(BIN) from $(DOWNLOAD).

endef

build: clean-all $(PUBLISH_DIR)/README.md
ifndef GEN
	$(error $(BINARY_ERROR))
endif
	@echo ">> Rebuilding book ..."
	@$(GEN) build -d $(PUBLISH_CONTENT)

serve:
	@echo ">> Preparing to run mdbook server ..."
	@$(GEN) serve -p $(PORT) -d $(PUBLISH_CONTENT)

run: serve

clean:
	@echo ">> Removing auto-generated top-level files ..."
	@rm -f $(PUBLISH_DIR)/README.md

clean-all: clean
	@echo ">> Removing previously generated content ..."
	@rm -rf $(PUBLISH_CONTENT)

book-submodule:
	@git submodule add -b master `git remote get-url --push origin` $(PUBLISH_DIR)
	@git commit --author "LFE Maintainers <maintainers@lfe.io>" \
		-m "Added master branch as submodule ($(PUBLISH_DIR) dir)."

book-init:
	@git submodule update --init --recursive && \
	cd $(PUBLISH_DIR) && \
	git checkout master

$(PUBLISH_DIR)/README.md:
	@echo '# Content for SICP, the LFE Edition' > $(PUBLISH_DIR)/README.md
	@echo 'Published at [lfe.io/books/sicp/](https://lfe.io/books/sicp/)' >> $(PUBLISH_DIR)/README.md
	@cd $(PUBLISH_DIR) && git add README.md

publish: build
	@echo ">> Publishing book content ..."
	-@cd $(PUBLISH_DIR) && \
	git add * && \
	git commit --author "LFE Maintainers <maintainers@lfe.io>" \
		-am "Regenerated book content." > /dev/null && \
	git push origin $(PUBLISH_BRANCH)
	-@git add $(PUBLISH_DIR) && \
	git commit --author "LFE Maintainers <maintainers@lfe.io>" \
		-am "Updated submodule for recently generated book content." && \
	git submodule update
	-@git push origin $(BUILDER_BRANCH)

build-publish: build publish

spell-check:
	@for FILE in `find . -name "*.md"`; do \
	RESULTS=$$(cat $$FILE | aspell -d en_GB --mode=markdown list | sort -u | sed -e ':a' -e 'N;$$!ba' -e 's/\n/, /g'); \
	if [[ "$$RESULTS" != "" ]] ; then \
	echo "Potential spelling errors in $$FILE:"; \
	echo "$$RESULTS" | \
	sed -e 's/^/    /'; \
	echo; \
	fi; \
	done

add-word: WORD ?= ""
add-word:
	@echo "*$(WORD)\n#" | aspell -a > /dev/null

add-words: WORDS ?= ""
add-words:
	@echo "Adding words:"
	@for WORD in `echo $(WORDS)| tr "," "\n"| tr "," "\n" | sed -e 's/^[ ]*//' | sed -e 's/[ ]*$$//'`; \
	do echo "  $$WORD ..."; \
	echo "*$$WORD\n#" | aspell -a > /dev/null; \
	done
	@echo

spell-suggest:
	@for FILE in `find . -name "*.md"`; do \
	RESULTS=$$(cat $$FILE | aspell -d en_GB --mode=markdown list | sort -u ); \
	if [[ "$$RESULTS" != "" ]] ; then \
	echo "Potential spelling errors in $$FILE:"; \
	for WORD in $$RESULTS; do \
	echo $$WORD| aspell -d en_GB pipe | tail -2|head -1 | sed -e 's/^/    /'; \
	done; \
	echo; \
	fi; \
	done
