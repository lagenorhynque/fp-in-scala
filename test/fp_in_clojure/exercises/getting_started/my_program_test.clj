(ns fp-in-clojure.exercises.getting-started.my-program-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.test.check.clojure-test :as tc]
   [clojure.test.check.properties :as prop]
   [fp-in-clojure.exercises.getting-started.my-program :as sut]))

(tc/defspec factorial-test 1000
  (prop/for-all [n (s/gen (s/and pos-int?
                                 #(<= % 1000)))]
    (= (apply *' (range 1 (inc n)))
       (sut/factorial n))))

(tc/defspec factorial2-test 1000
  (prop/for-all [n (s/gen (s/and pos-int?
                                 #(<= % 1000)))]
    (= (apply *' (range 1 (inc n)))
       (sut/factorial2 n))))
