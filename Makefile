# decel - Google CEL for D
# ========================

DUB      ?= dub
DFMT     ?= dfmt
DSCANNER ?= dscanner

# Directories
BUILD_DIR  := build
SOURCE_DIR := source

.PHONY: all build test unittest format format-check lint clean help

# ── Default ──────────────────────────────────────────────────────────

all: build

# ── Build ────────────────────────────────────────────────────────────

build: ## Build the library
	@$(DUB) build -q

# ── Testing ──────────────────────────────────────────────────────────

test: unittest lint format-check ## Run all checks (tests + lint + format)

unittest: ## Run unit tests via unit-threaded
	@$(DUB) test -q

# ── Formatting ───────────────────────────────────────────────────────

format: ## Format source code with dfmt
	@find $(SOURCE_DIR) -name '*.d' | xargs $(DFMT) -i

format-check: ## Check formatting without modifying files
	@fail=0; \
	for f in $$(find $(SOURCE_DIR) -name '*.d'); do \
		if ! $(DFMT) < "$$f" | diff -q "$$f" - > /dev/null 2>&1; then \
			echo "Needs formatting: $$f"; \
			fail=1; \
		fi; \
	done; \
	if [ "$$fail" = "1" ]; then \
		echo "Run 'make format' to fix."; \
		exit 1; \
	fi

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
