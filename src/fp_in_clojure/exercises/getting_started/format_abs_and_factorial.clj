(ns fp-in-clojure.exercises.getting-started.format-abs-and-factorial
  (:require
   [fp-in-clojure.exercises.getting-started.my-program :refer [abs factorial format-result]]))

;; Now we can use our general `format-result` function
;; with both `abs` and `factorial`
(defn -main [& _]
  (println (format-result "absolute value" -42 abs))
  (println (format-result "factorial" 7 factorial)))

(comment
  (with-out-str
    (-main))
  )
