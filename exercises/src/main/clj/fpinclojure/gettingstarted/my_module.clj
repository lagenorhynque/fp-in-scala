;; A comment!
;;; Another comment
(ns fpinclojure.gettingstarted.my-module
  "A documentation comment"
  (:refer-clojure :exclude [abs])
  (:require
   [clojure.spec.alpha :as s]))

(s/fdef abs
  :args (s/cat :n integer?)
  :ret integer?)

(defn abs [n]
  (if (neg? n)
    (- n)
    n))

(defn- format-abs [x]
  (let [msg "The absolute value of %d is %d"]
    (format msg x (abs x))))

(defn -main [& _]
  (println (format-abs -42)))

;; A definition of factorial, using a local, tail recursive function

(s/fdef factorial
  :args (s/cat :n integer?)
  :ret integer?)

(defn factorial [n]
  (loop [n n
         acc 1]
    (if (<= n 0)
      acc
      (recur (dec n)
             (*' n acc)))))

;; Another implementation of `factorial`, this time with a `while` loop

(s/fdef factorial2
  :args (s/cat :n integer?)
  :ret integer?)

(defn factorial2 [n]
  (let [acc (volatile! 1)
        i (volatile! n)]
    (while (pos? @i)
      (vswap! acc *' @i)
      (vswap! i dec))
    @acc))

;; Exercise 2.1: Write a function to compute the nth fibonacci number

(s/fdef fib
  :args (s/cat :n integer?)
  :ret integer?)

(defn fib [n]
  (loop [n n
         prev 0
         curr 1]
    (if (<= n 0)
      prev
      (recur (dec n)
             curr
             (+' prev curr)))))

;; This definition and `format-abs` are very similar..

(defn- format-factorial [n]
  (let [msg "The factorial of %d is %d."]
    (format msg n (factorial n))))

;; We can generalize `format-abs` and `format-factorial` to
;; accept a _function_ as a parameter

(s/fdef format-result
  :args (s/cat :name string?
               :n integer?
               :f (s/fspec :args (s/tuple integer?)
                           :ret integer?))
  :ret string?)

(defn format-result [name n f]
  (let [msg "The %s of %d is %d."]
    (format msg name n (f n))))

(comment
  (require '[clojure.spec.test.alpha :as stest])
  (stest/instrument)

  (with-out-str
    (-main))

  (abs -42)

  (format-abs -42)

  (map factorial (range 10))

  (map factorial2 (range 10))

  (map fib (range 10))

  (format-factorial 6)

  (format-result "absolute value" -42 abs)

  (format-result "factorial" 7 factorial)

  (format-result "factorial" 7 fib)
  )
