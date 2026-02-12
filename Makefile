# decel - Google CEL for D
# ========================

DUB      ?= dub
DFMT     ?= dfmt
DSCANNER ?= dscanner

# Directories
BUILD_DIR  := build
SOURCE_DIR := source

.PHONY: all build test unittest format lint clean help

# ── Default ──────────────────────────────────────────────────────────

all: build

# ── Build ────────────────────────────────────────────────────────────

build: ## Build the library
	@$(DUB) build -q

# ── Testing ──────────────────────────────────────────────────────────

test: format unittest lint ## Run all checks (format + tests + lint)

unittest: ## Run unit tests via unit-threaded
	@$(DUB) test -q

# ── Formatting ───────────────────────────────────────────────────────

format: ## Format source code with dfmt (shows diff of changes)
	@for f in $$(find $(SOURCE_DIR) -name '*.d'); do \
		before=$$(cat "$$f"); \
		$(DFMT) -i "$$f"; \
		after=$$(cat "$$f"); \
		if [ "$$before" != "$$after" ]; then \
			echo "dfmt reformatted: $$f"; \
		fi; \
	done



# ── Linting ──────────────────────────────────────────────────────────

lint: ## Run dscanner static analysis
	@$(DSCANNER) --styleCheck $(SOURCE_DIR)/

# ── Cleanup ──────────────────────────────────────────────────────────

clean: ## Remove build artifacts
	@rm -rf $(BUILD_DIR) .dub

# ── Help ─────────────────────────────────────────────────────────────

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  %-15s %s\n", $$1, $$2}'
