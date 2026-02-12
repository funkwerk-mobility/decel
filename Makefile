# decel - Google CEL for D
# ========================

DUB      ?= dub
DFMT     ?= dfmt
DSCANNER ?= dscanner

# Directories
BUILD_DIR  := build
SOURCE_DIR := source

# Colors for output
GREEN  := \033[0;32m
RED    := \033[0;31m
YELLOW := \033[0;33m
RESET  := \033[0m

.PHONY: all build test unittest format format-check lint clean help

# ── Default ──────────────────────────────────────────────────────────

all: build

# ── Build ────────────────────────────────────────────────────────────

build: ## Build the library
	@echo "$(GREEN)Building decel...$(RESET)"
	$(DUB) build

# ── Testing ──────────────────────────────────────────────────────────

test: unittest lint format-check ## Run all checks (tests + lint + format)
	@echo "$(GREEN)All checks passed!$(RESET)"

unittest: ## Run unit tests via unit-threaded
	@echo "$(GREEN)Running unit tests...$(RESET)"
	$(DUB) test

# ── Formatting ───────────────────────────────────────────────────────

format: ## Format source code with dfmt
	@echo "$(YELLOW)Formatting...$(RESET)"
	find $(SOURCE_DIR) -name '*.d' | xargs $(DFMT) -i

format-check: ## Check formatting without modifying files
	@echo "$(YELLOW)Checking formatting...$(RESET)"
	@find $(SOURCE_DIR) -name '*.d' | xargs $(DFMT) --check && \
		echo "$(GREEN)Formatting OK$(RESET)" || \
		(echo "$(RED)Formatting issues found. Run 'make format' to fix.$(RESET)" && exit 1)

# ── Linting ──────────────────────────────────────────────────────────

lint: ## Run dscanner static analysis
	@echo "$(YELLOW)Running dscanner...$(RESET)"
	$(DSCANNER) --styleCheck $(SOURCE_DIR)/ && \
		echo "$(GREEN)Lint OK$(RESET)" || \
		(echo "$(RED)Lint issues found.$(RESET)" && exit 1)

# ── Cleanup ──────────────────────────────────────────────────────────

clean: ## Remove build artifacts
	@echo "$(YELLOW)Cleaning...$(RESET)"
	rm -rf $(BUILD_DIR) .dub
	@echo "$(GREEN)Clean!$(RESET)"

# ── Help ─────────────────────────────────────────────────────────────

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  $(GREEN)%-15s$(RESET) %s\n", $$1, $$2}'