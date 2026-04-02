(ns fpinclojure.exercises.datastructures.tree
  (:refer-clojure :exclude [map])
  (:require
   [clojure.spec.alpha :as s]))

(defrecord Leaf [value])
(defrecord Branch [left right])

(s/fdef leaf?
  :args (s/cat :x any?)
  :ret boolean?)

(defn leaf? [x]
  (instance? Leaf x))

(s/fdef branch?
  :args (s/cat :x any?)
  :ret boolean?)

(defn branch? [x]
  (and (instance? Branch x)
       ((some-fn leaf? branch?) (:left x))
       ((some-fn leaf? branch?) (:right x))))

(s/def ::tree
  (s/or :leaf leaf?
        :branch branch?))

(s/fdef size
  :args (s/cat :t ::tree)
  :ret int?)

(defn size [t]
  (if (leaf? t)
    1
    (+ (-> t :left size)
       1
       (-> t :right size))))

;; Exercise 3.25

(s/fdef maximum
  :args (s/cat :t ::tree)
  :ret any?)

(defn maximum [t]
  (if (leaf? t)
    (:value t)
    (max (-> t :left maximum)
         (-> t :right maximum))))

;; Exercise 3.26

(s/fdef depth
  :args (s/cat :t ::tree)
  :ret int?)

(defn depth [t]
  (if (leaf? t)
    0
    (inc (max (-> t :left depth)
              (-> t :right depth)))))

;; Exercise 3.27

(s/fdef map
  :args (s/cat :f ifn?
               :t ::tree)
  :ret ::tree)

(defn map [f t]
  (if (leaf? t)
    (-> t :value f ->Leaf)
    (->Branch (map f (:left t))
              (map f (:right t)))))

;; Exercise 3.28

(s/fdef fold
  :args (s/cat :f ifn?
               :g ifn?
               :t ::tree)
  :ret any?)

(defn fold [f g t]
  (if (leaf? t)
    (-> t :value f)
    (g (fold f g (:left t))
       (fold f g (:right t)))))

(s/fdef size-via-fold
  :args (s/cat :t ::tree)
  :ret int?)

(defn size-via-fold [t]
  (fold (constantly 1) #(+ %1 1 %2) t))

(s/fdef maximum-via-fold
  :args (s/cat :t ::tree)
  :ret any?)

(defn maximum-via-fold [t]
  (fold identity max t))

(s/fdef depth-via-fold
  :args (s/cat :t ::tree)
  :ret int?)

(defn depth-via-fold [t]
  (fold (constantly 0) #(inc (max %1 %2)) t))

(s/fdef map-via-fold
  :args (s/cat :f ifn?
               :t ::tree)
  :ret ::tree)

(defn map-via-fold [f t]
  (fold #(-> % f ->Leaf) ->Branch t))

(comment
  (require '[clojure.spec.test.alpha :as stest])
  (stest/instrument)

  (size (->Leaf 2))
  (size (->Branch (->Leaf 2)
                  (->Leaf 4)))
  (size (->Branch (->Leaf 1)
                  (->Branch (->Leaf 4)
                            (->Leaf 2))))

  (maximum (->Leaf 2))
  (maximum (->Branch (->Leaf 2)
                     (->Leaf 4)))
  (maximum (->Branch (->Leaf 1)
                     (->Branch (->Leaf 4)
                               (->Leaf 2))))

  (depth (->Leaf 2))
  (depth (->Branch (->Leaf 2)
                   (->Leaf 4)))
  (depth (->Branch (->Leaf 1)
                   (->Branch (->Leaf 4)
                             (->Leaf 2))))

  (map #(* % %) (->Leaf 2))
  (map #(* % %) (->Branch (->Leaf 2)
                          (->Leaf 4)))
  (map #(* % %) (->Branch (->Leaf 1)
                          (->Branch (->Leaf 4)
                                    (->Leaf 2))))

  (size-via-fold (->Leaf 2))
  (size-via-fold (->Branch (->Leaf 2)
                           (->Leaf 4)))
  (size-via-fold (->Branch (->Leaf 1)
                           (->Branch (->Leaf 4)
                                     (->Leaf 2))))
  (maximum-via-fold (->Leaf 2))
  (maximum-via-fold (->Branch (->Leaf 2)
                              (->Leaf 4)))
  (maximum-via-fold (->Branch (->Leaf 1)
                              (->Branch (->Leaf 4)
                                        (->Leaf 2))))
  (depth-via-fold (->Leaf 2))
  (depth-via-fold (->Branch (->Leaf 2)
                            (->Leaf 4)))
  (depth-via-fold (->Branch (->Leaf 1)
                            (->Branch (->Leaf 4)
                                      (->Leaf 2))))
  (map-via-fold #(* % %) (->Leaf 2))
  (map-via-fold #(* % %) (->Branch (->Leaf 2)
                                   (->Leaf 4)))
  (map-via-fold #(* % %) (->Branch (->Leaf 1)
                                   (->Branch (->Leaf 4)
                                             (->Leaf 2))))
  )
