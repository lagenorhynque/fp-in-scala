.PHONY: lint
lint: lint-clj lint-scala lint-flix

# Clojure

.PHONY: lint-clj
lint-clj: cljstyle-check clj-kondo-lint joker-lint

.PHONY: cljstyle-check
cljstyle-check:
	@cljstyle check

.PHONY: cljstyle-fix
cljstyle-fix:
	@cljstyle fix

.PHONY: clj-kondo-lint
clj-kondo-lint:
	@clj-kondo --lint src test

.PHONY: joker-lint
joker-lint:
	@joker --lint --working-dir .

# Scala

.PHONY: lint-scala
lint-scala: scalafmt-check scalafix-lint

.PHONY: scalafmt-check
scalafmt-check:
	@scalafmt --check src

.PHONY: scalafmt-fix
scalafmt-fix:
	@scalafmt src

.PHONY: scalafix-lint
scalafix-lint:
	@scalafix --check src

# Flix

.PHONY: lint-flix
lint-flix: check-flix

.PHONY: flix-check
check-flix:
	@flix check
