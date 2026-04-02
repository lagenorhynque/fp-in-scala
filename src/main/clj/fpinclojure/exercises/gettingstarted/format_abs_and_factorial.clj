(ns fpinclojure.exercises.gettingstarted.format-abs-and-factorial
  (:require
   [fpinclojure.exercises.gettingstarted.my-program :refer [abs factorial format-result]]))

;; Now we can use our general `format-result` function
;; with both `abs` and `factorial`
(defn -main [& _]
  (println (format-result "absolute value" -42 abs))
  (println (format-result "factorial" 7 factorial)))

(comment
  (with-out-str
    (-main))
  )
