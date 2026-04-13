(ns fp-in-clojure.exercises.getting-started.polymorphic-functions
  (:refer-clojure :exclude [sorted?])
  (:require
   [clojure.spec.alpha :as s]))

;; Here's a polymorphic version of `find-first`, parameterized on
;; a function for testing whether an `any?` is the element we want to find.
;; Instead of hard-coding `string?`, we take a type `any?` as a parameter.
;; And instead of hard-coding an equality check for a given key,
;; we take a function with which to test each element of the sequence.

(s/fdef find-first
  :args (s/cat :as (s/coll-of any?)
               :p ifn?)
  :ret int?)

(defn find-first [as p]
  (loop [n 0]
    (cond
      (>= n (count as)) -1
      ;; If the function `p` matches the current element,
      ;; we've found a match and we return its index in the sequence.
      (p (nth as n)) n
      :else (recur (inc n)))))

;; Exercise 2.2: Implement a polymorphic function to check whether
;; a sequnece is sorted

(s/fdef sorted?
  :args (s/cat :as (s/coll-of any?)
               :gt ifn?)
  :ret boolean?)

(defn sorted? [as gt]
  (loop [n 0]
    (cond
      (>= n (dec (count as))) true
      (gt (nth as n) (nth as (inc n))) false
      :else (recur (inc n)))))

;; Polymorphic functions are often so constrained by their type
;; that they only have one implementation! Here's an example:

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

  (sorted? [2 5 1 4 3] >)

  (sorted? [1 2 3 4 5] >)

  (sorted? [1 1 3 4 5] >)

  ((partial1 1 #(+ %1 %2)) 2)

  (((curry #(+ %1 %2)) 1) 2)

  ((uncurry (fn [a] (fn [b] (+ a b)))) 1 2)

  ((compose #(* % 2) #(* % % %)) 3)

  ((compose #(* % % %) #(* % 2)) 3)
  )
