(ns fpinclojure.exercises.datastructures.list
  (:refer-clojure :exclude [concat drop drop-while filter list map reverse])
  (:require
   [clojure.core.match :refer [match]]
   [clojure.spec.alpha :as s])
  (:import
   (clojure.lang
    ISeq
    Sequential)))

(deftype Cons
  [head tail]
  ISeq
  (first [_] head)
  (next [_] tail)
  (more [_] tail)
  (cons [this o] (Cons. o this))
  (equiv [this o]
    (and (sequential? o)
         (= (first this) (first o))
         (= (next this) (next o))))
  (seq [this] this)
  Sequential)

(s/fdef cons?
  :args (s/cat :x any?)
  :ret boolean?)

(defn cons? [x]
  (and (instance? Cons x)
       (or (nil? (.-tail x))
           (cons? (.-tail x)))))

(s/def ::list
  (s/nilable cons?))

(s/fdef list
  :args (s/cat :args (s/* any?))
  :ret ::list)

(defn list [& args]
  (if (empty? args)
    nil
    (Cons. (first args)
           (apply list (rest args)))))

(s/fdef sum
  :args (s/cat :ns (s/and ::list
                          #(every? integer? %)))
  :ret integer?)

(defn sum [ns]
  (if (empty? ns)
    0
    (+ (first ns)
       (sum (rest ns)))))

(s/fdef product
  :args (s/cat :ns (s/and ::list
                          #(every? integer? %)))
  :ret integer?)

(defn product [ns]
  (cond
    (empty? ns) 1
    (zero? (first ns)) 0
    :else (* (first ns)
             (product (rest ns)))))

;; Exercise 3.1
(def result
  ;; 準標準ライブラリcore.matchを利用すると
  (match (list 1 2 3 4 5)
    ([x 2 4 & _] :seq) x
    nil 42
    ([x y 3 4 & _] :seq) (+ x y)
    ([h & t] :seq) (+ h (sum t))
    :else 101))  ; => 3

(s/fdef append
  :args (s/cat :a1 ::list
               :a2 ::list)
  :ret ::list)

(defn append [a1 a2]
  (if (empty? a1)
    a2
    (Cons. (first a1)
           (append (rest a1) a2))))

(s/fdef fold-right
  :args (s/cat :f ifn?
               :acc any?
               :as ::list)
  :ret any?)

(defn fold-right [f acc as]
  (if (empty? as)
    acc
    (f (first as)
       (fold-right f acc (rest as)))))

(s/fdef sum-via-fold-right
  :args (s/cat :ns (s/and ::list
                          #(every? integer? %)))
  :ret integer?)

(defn sum-via-fold-right [ns]
  (fold-right + 0 ns))

(s/fdef product-via-fold-right
  :args (s/cat :ns (s/and ::list
                          #(every? integer? %)))
  :ret integer?)

(defn product-via-fold-right [ns]
  (fold-right * 1 ns))

;; Exercise 3.2

(s/fdef tail
  :args (s/cat :l ::list)
  :ret ::list)

(defn tail [l]
  (if (empty? l)
    nil
    (rest l)))

;; 入力が nil の場合の実装上のその他の選択肢:
;; - エラー終了する(例外をスローする)

;; Exercise 3.3

(s/fdef set-head
  :args (s/cat :l ::list
               :h any?)
  :ret ::list)

(defn set-head [l h]
  (if (empty? l)
    (Cons. h nil)
    (Cons. h (rest l))))

;; Exercise 3.4

(s/fdef drop
  :args (s/cat :n integer?
               :l ::list)
  :ret ::list)

(defn drop [n l]
  (cond
    (<= n 0) l
    (empty? l) nil
    :else (recur (dec n) (rest l))))

;; Exercise 3.5

(s/fdef drop-while
  :args (s/cat :f ifn?
               :l ::list)
  :ret ::list)

(defn drop-while [f l]
  (cond
    (empty? l) nil
    (f (first l)) (recur f (rest l))
    :else l))

;; Exercise 3.6

(s/fdef init
  :args (s/cat :l ::list)
  :ret ::list)

(defn init [l]
  (if (or (empty? l)
          (empty? (rest l)))
    nil
    (Cons. (first l) (init (rest l)))))

;; このリストの実装は単方向連結リストであり、最終要素を除外したリストを得るにはリスト全体を走査し再構築する必要がある。

;; Exercise 3.7

;; この foldRight の実装では入力のリストを最終要素まで評価しなければならないため、特定の条件で再帰を中止して値を返すようなことはできない。

;; Exercise 3.8

(comment
  ;; fold-right の引数 acc に Nil 、f に #(Cons %1 %2) を指定すると、もとのリストがそのまま得られる。

  (= (fold-right #(Cons. %1 %2) nil (list 1 2 3))
     (list 1 2 3))

  ;; fold-right とはリストのデータコンストラクタ nil, Cons を引数 acc, f で置き換える操作と考えることができる。
  )

;; Exercise 3.9

(s/fdef length
  :args (s/cat :l ::list)
  :ret int?)

(defn length [l]
  (fold-right #(inc %2) 0 l))

;; Exercise 3.10

(s/fdef fold-left
  :args (s/cat :f ifn?
               :acc any?
               :as ::list)
  :ret any?)

(defn fold-left [f acc as]
  (if (empty? as)
    acc
    (recur f
           (f acc (first as))
           (rest as))))

;; Exercise 3.11

(s/fdef sum-via-fold-left
  :args (s/cat :ns (s/and ::list
                          #(every? integer? %)))
  :ret integer?)

(defn sum-via-fold-left [ns]
  (fold-left + 0 ns))

(s/fdef product-via-fold-left
  :args (s/cat :ns (s/and ::list
                          #(every? integer? %)))
  :ret integer?)

(defn product-via-fold-left [ns]
  (fold-left * 1 ns))

(s/fdef length-via-fold-left
  :args (s/cat :l ::list)
  :ret int?)

(defn length-via-fold-left [l]
  (fold-left (fn [c _] (inc c)) 0 l))

;; Exercise 3.12

(s/fdef reverse
  :args (s/cat :l ::list)
  :ret ::list)

(defn reverse [l]
  (fold-left #(Cons. %2 %1) nil l))

;; Exercise 3.13

(s/fdef fold-left2
  :args (s/cat :f ifn?
               :acc any?
               :as ::list)
  :ret any?)

(defn fold-left2 [f acc as]
  (fold-right #(f %2 %1) acc (reverse as)))

(s/fdef fold-right2
  :args (s/cat :f ifn?
               :acc any?
               :as ::list)
  :ret any?)

(defn fold-right2 [f acc as]
  (fold-left #(f %2 %1) acc (reverse as)))

;; Exercise 3.14

(s/fdef append-via-fold-right
  :args (s/cat :a1 ::list
               :a2 ::list)
  :ret ::list)

(defn append-via-fold-right [a1 a2]
  (fold-right #(Cons. %1 %2) a2 a1))

;; Exercise 3.15

(s/fdef concat
  :args (s/cat :ls (s/and ::list
                          #(every? (some-fn nil? cons?) %)))
  :ret ::list)

(defn concat [ls]
  (fold-right append nil ls))

;; Exercise 3.16

(s/fdef increment-each
  :args (s/cat :ns (s/and ::list
                          #(every? integer? %)))
  :ret (s/and ::list
              #(every? integer? %)))

(defn increment-each [ns]
  (fold-right #(Cons. (inc %1) %2) nil ns))

;; Exercise 3.17

(s/fdef double-to-string
  :args (s/cat :ns (s/and ::list
                          #(every? double? %)))
  :ret (s/and ::list
              #(every? string? %)))

(defn double-to-string [ns]
  (fold-right #(Cons. (str %1) %2) nil ns))

;; Exercise 3.18

(s/fdef map
  :args (s/cat :f ifn?
               :l ::list)
  :ret ::list)

(defn map [f l]
  (fold-right #(Cons. (f %1) %2) nil l))

;; Exercise 3.19

(s/fdef filter
  :args (s/cat :f ifn?
               :l ::list)
  :ret ::list)

(defn filter [f l]
  (fold-right (fn [x acc] (if (f x) (Cons. x acc) acc)) nil l))

;; Exercise 3.20

(s/fdef flat-map
  :args (s/cat :f ifn?
               :l ::list)
  :ret ::list)

(defn flat-map [f l]
  (concat (map f l)))

;; Exercise 3.21

(s/fdef filter-via-flat-map
  :args (s/cat :f ifn?
               :l ::list)
  :ret ::list)

(defn filter-via-flat-map [f l]
  (flat-map (fn [x] (if (f x) (list x) nil)) l))

;; Exercise 3.22

(s/fdef add-pairwise
  :args (s/cat :a1 (s/and ::list
                          #(every? integer? %))
               :a2 (s/and ::list
                          #(every? integer? %)))
  :ret (s/and ::list
              #(every? integer? %)))

(defn add-pairwise [a1 a2]
  (if (or (empty? a1)
          (empty? a2))
    nil
    (Cons. (+ (first a1) (first a2))
           (add-pairwise (rest a1) (rest a2)))))

;; Exercise 3.23

(s/fdef zip-with
  :args (s/cat :f ifn?
               :as ::list
               :bs ::list)
  :ret ::list)

(defn zip-with [f as bs]
  (if (or (empty? as)
          (empty? bs))
    nil
    (Cons. (f (first as) (first bs))
           (zip-with f (rest as) (rest bs)))))

;; Exercise 3.24

(defn- starts-with [l prefix]
  (cond
    (empty? prefix) true
    (= (first l) (first prefix)) (recur (rest l) (rest prefix))
    :else false))

(s/fdef has-subsequence
  :args (s/cat :sup ::list
               :sub ::list)
  :ret boolean?)

(defn has-subsequence [sup sub]
  (cond
    (empty? sup) (empty? sub)
    (starts-with sup sub) true
    :else (recur (rest sup) sub)))

(comment
  (require '[clojure.spec.test.alpha :as stest])
  (stest/instrument)

  (sum (list 1 2 4))
  (sum (list 2))
  (sum nil)

  (product (list 1 2 4))
  (product (list 2))
  (product nil)

  (append (list 1 2) (list 3 4 5))
  (append nil (list 3 4 5))
  (append (list 1 2) nil)

  (sum-via-fold-right (list 1 2 4))
  (sum-via-fold-right (list 2))
  (sum-via-fold-right nil)

  (product-via-fold-right (list 1 2 4))
  (product-via-fold-right (list 2))
  (product-via-fold-right nil)

  (tail (list 1 2 3))
  (tail (list 2))
  (tail nil)

  (set-head (list 1 2 3) 4)
  (set-head (list 2) 4)
  (set-head nil 4)

  (drop 1 (list 1 2 3))
  (drop 3 (list 1 2 3))
  (drop 4 (list 1 2 3))

  (drop-while #(< % 2) (list 1 2 3))
  (drop-while #(< % 3) (list 1 2 3))
  (drop-while #(< % 4) (list 1 2 3))

  (init (list 1 2 3))
  (init (list 2))
  (init nil)

  (length (list :a :b :c))
  (length (list :a))
  (length nil)

  (sum-via-fold-left (list 1 2 4))
  (sum-via-fold-left (list 2))
  (sum-via-fold-left nil)
  (product-via-fold-left (list 1 2 4))
  (product-via-fold-left (list 2))
  (product-via-fold-left nil)
  (length-via-fold-left (list :a :b :c))
  (length-via-fold-left (list :a))
  (length-via-fold-left nil)

  (reverse (list 1 2 3))

  (= (fold-left - 0 (list 1 2 3))
     (fold-left2 - 0 (list 1 2 3)))
  (= (fold-right - 0 (list 1 2 3))
     (fold-right2 - 0 (list 1 2 3)))

  (append-via-fold-right (list 1 2) (list 3 4 5))
  (append-via-fold-right nil (list 3 4 5))
  (append-via-fold-right (list 1 2) nil)

  (concat (list (list 1 2) (list 3 4 5) (list 6)))
  (concat (list (list 3 4 5)))

  (increment-each (list 1 2 3))

  (double-to-string (list 1.2 2.3 3.4))

  (map #(* % %) (list 1 2 3))

  (filter odd? (list 1 2 3))

  (flat-map #(list % %) (list 1 2 3))

  (filter-via-flat-map odd? (list 1 2 3))

  (add-pairwise (list 1 2 3) (list 4 5))
  (add-pairwise (list 1 2) (list 3 4 5))

  (zip-with * (list 1 2 3) (list 4 5))
  (zip-with * (list 1 2) (list 3 4 5))

  (has-subsequence (list 1 2 3 4 5) (list 1 2 3))
  (has-subsequence (list 1 2 3 4 5) (list 3 4))
  (has-subsequence (list 1 2 3 4 5) (list 5))
  (has-subsequence (list 1 2 3 4 5) (list 1 3))
  )
