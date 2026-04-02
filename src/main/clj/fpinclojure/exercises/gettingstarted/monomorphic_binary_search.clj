(ns fpinclojure.exercises.gettingstarted.monomorphic-binary-search
  (:require
   [clojure.spec.alpha :as s]))

(s/fdef find-first
  :args (s/cat :ss (s/coll-of string?)
               :k string?)
  :ret int?)

(defn find-first [ss k]
  (loop [n 0]
    (cond
      (>= n (count ss)) -1
      (= (nth ss n) k) n
      :else (recur (inc n)))))

(comment
  (require '[clojure.spec.test.alpha :as stest])
  (stest/instrument)

  (find-first ["b" "e" "a" "d" "c"] "b")

  (find-first ["b" "e" "a" "d" "c"] "d")

  (find-first ["b" "e" "a" "d" "c"] "c")

  (find-first ["b" "e" "a" "d" "c"] "f")

  (find-first [] "b")
  )
