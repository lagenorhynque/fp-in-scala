(ns fp-in-clojure.exercises.getting-started.monomorphic-binary-search
  (:require
   [clojure.spec.alpha :as s]))

;; First, a find-first, specialized to `string?``.
;; Ideally, we could generalize this to work for any `seqable?` type.

(s/fdef find-first
  :args (s/cat :ss (s/coll-of string?)
               :k string?)
  :ret int?)

(defn find-first [ss k]
  ;; Start the loop at the first element of the sequence.
  (loop [n 0]
    (cond
      ;; If `n` is past the end of the sequence, return `-1`
      ;; indicating the key doesn't exist in the sequence.
      (>= n (count ss)) -1
      ;; `(nth ss n)` extracts the n'th element of the sequence `ss`.
      ;; If the element at `n` is equal to the key, return `n`
      ;; indicating that the element appears in the sequence at that index.
      (= (nth ss n) k) n
      :else (recur (inc n)))))  ; Otherwise increment `n` and keep looking.

(comment
  (require '[clojure.spec.test.alpha :as stest])
  (stest/instrument)

  (find-first ["b" "e" "a" "d" "c"] "b")

  (find-first ["b" "e" "a" "d" "c"] "d")

  (find-first ["b" "e" "a" "d" "c"] "c")

  (find-first ["b" "e" "a" "d" "c"] "f")

  (find-first [] "b")
  )
