.PHONY: cljstyle-check
cljstyle-check:
	@cljstyle check

.PHONY: cljstyle-fix
cljstyle-fix:
	@cljstyle fix

.PHONY: clj-kondo-lint
clj-kondo-lint:
	@clj-kondo --lint src

.PHONY: joker-lint
joker-lint:
	@joker --lint --working-dir .

.PHONY: lint-clj
lint-clj:
	@make cljstyle-check clj-kondo-lint joker-lint
