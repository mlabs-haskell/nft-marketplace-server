build: requires_nix_shell
	cabal build

NIX_SOURCES := $(shell fd -enix)
CABAL_SOURCES := $(shell fd -ecabal)
HASKELL_SOURCES := $(shell fd -ehs)
GHC_FLAGS := $(shell echo "-o -XImportQualifiedPost -o -XTypeApplications")

format: requires_nix_shell
	fourmolu --mode inplace --check-idempotence $(GHC_FLAGS) $(HASKELL_SOURCES)
	nixpkgs-fmt $(NIX_SOURCES)
	cabal-fmt -i $(CABAL_SOURCES)

# Check formatting (without making changes)
format_check: requires_nix_shell
	fourmolu --mode check --check-idempotence $(GHC_FLAGS) $(HASKELL_SOURCES)
	nixpkgs-fmt --check $(NIX_SOURCES)
	cabal-fmt -c $(CABAL_SOURCES)

# Apply hlint suggestions
lint: requires_nix_shell
	find -name '*.hs' -not -path './dist-*/*' -exec hlint --refactor --refactor-options="--inplace" {} \;

# Check hlint suggestions
lint_check: requires_nix_shell
	hlint $(shell fd -ehs)

# Target to use as dependency to fail if not inside nix-shell
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"
	@ [ "$(IN_NIX_SHELL)" ] || (echo "    run 'nix develop' first" && false)
