(ns fp-in-clojure.exercises.getting-started.polymorphic-functions
  (:require
   [clojure.spec.alpha :as s]))

(s/fdef find-first
  :args (s/cat :as (s/coll-of any?)
               :p ifn?)
  :ret int?)

(defn find-first [as p]
  (loop [n 0]
    (cond
      (>= n (count as)) -1
      (p (nth as n)) n
      :else (recur (inc n)))))

;; Exercise 2.2: Implement a polymorphic function to check whether
;; a sequnece is sorted

(s/fdef is-sorted
  :args (s/cat :as (s/coll-of any?)
               :ordered ifn?)
  :ret boolean?)

(defn is-sorted [as ordered]
  (loop [n 0]
    (cond
      (>= n (dec (count as))) true
      (not (ordered (nth as n) (nth as (inc n)))) false
      :else (recur (inc n)))))

(s/fdef partial1
  :args (s/cat :a any?
               :f ifn?)
  :ret ifn?)

(defn partial1 [a f]
  (fn [b] (f a b)))

;; Exercise 2.3: Implement `curry`.

(s/fdef curry
  :args (s/cat :f ifn?)
  :ret ifn?)

(defn curry [f]
  (fn [a] (fn [b] (f a b))))

;; Exercise 2.4: Implement `uncurry`

(s/fdef uncurry
  :args (s/cat :f ifn?)
  :ret ifn?)

(defn uncurry [f]
  (fn [a b] ((f a) b)))

;; Exercise 2.5: Implement `compose`

(s/fdef compose
  :args (s/cat :f ifn?
               :g ifn?)
  :ret ifn?)

(defn compose [f g]
  (fn [a] (-> a g f)))

(comment
  (require '[clojure.spec.test.alpha :as stest])
  (stest/instrument)

  (find-first [2 5 1 4 3] #(== % 2))

  (find-first [2 5 1 4 3] #(== % 4))

  (find-first [2 5 1 4 3] #(== % 3))

  (find-first [2 5 1 4 3] #(== % 0))

  (find-first [] #(== % 2))

  (is-sorted [2 5 1 4 3] <=)

  (is-sorted [1 2 3 4 5] <=)

  (is-sorted [1 1 3 4 5] <=)

  ((partial1 1 #(+ %1 %2)) 2)

  (((curry #(+ %1 %2)) 1) 2)

  ((uncurry (fn [a] (fn [b] (+ a b)))) 1 2)

  ((compose #(* % 2) #(* % % %)) 3)

  ((compose #(* % % %) #(* % 2)) 3)
  )
