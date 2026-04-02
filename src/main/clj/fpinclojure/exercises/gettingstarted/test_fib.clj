(ns fpinclojure.exercises.gettingstarted.test-fib
  (:require
   [fpinclojure.exercises.gettingstarted.my-program :refer [fib]]))

;; test implementation of `fib`
(defn -main [& _]
  (println "Expected: 0, 1, 1, 2, 3, 5, 8")
  (println (format "Actual:   %d, %d, %d, %d, %d, %d, %d"
                   (fib 0) (fib 1) (fib 2) (fib 3) (fib 4) (fib 5) (fib 6))))

(comment
  (with-out-str
    (-main))
  )
