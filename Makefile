.PHONY: viewer

help: ## Ask for help!
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

install: ## build binaries
	stack install

#####################
# Linting and Styling
#####################

stylish: ## Run stylish-haskell over all haskell projects
	find src tests \
	-name "*.hs" | xargs stack exec stylish-haskell -- -c ./.stylish_haskell.yaml -i
