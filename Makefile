.PHONY: lint
lint: lint-clj lint-scala lint-flix

.PHONY: test
test: test-clj test-scala test-flix

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

.PHONY: test-clj
test-clj:
	@clojure -T:build test

.PHONY: test-and-build-clj
test-and-build-clj:
	@clojure -T:build ci

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

.PHONY: compile-scala
compile-scala:
	@sbt compile

.PHONY: test-scala
test-scala:
	@sbt test

# Flix

.PHONY: lint-flix
lint-flix: check-flix

.PHONY: flix-check
check-flix:
	@flix check

.PHONY: compile-flix
compile-flix:
	@flix build

.PHONY: test-flix
test-flix:
	@flix test
