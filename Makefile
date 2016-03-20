STAGING_HOST=staging-docs.lfe.io
STAGING_PATH=/var/www/lfe/staging-docs/sicp

SRC=./
BASE_DIR=$(shell pwd)
PROD_DIR=_book
PROD_PATH=$(BASE_DIR)/$(PROD_DIR)
STAGE_DIR=$(PROD_DIR)
STAGE_PATH=$(BASE_DIR)/$(STAGE_DIR)

deps:
	@npm install -g gitbook-cli

setup:
	@gitbook install

build-book:
	gitbook build $(SRC) --output=$(PROD_DIR)

run-book:
	gitbook serve $(SRC)

staging: build
	git pull origin master && \
	rsync -azP ./$(STAGE_DIR)/* $(STAGING_HOST):$(STAGING_PATH)

publish: build
	-git commit -a && git push origin master
	git subtree push --prefix $(PROD_DIR) origin gh-pages

