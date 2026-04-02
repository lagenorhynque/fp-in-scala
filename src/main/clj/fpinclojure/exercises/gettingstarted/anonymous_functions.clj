;; Functions get passed around so often in FP that it's
;; convenient to have syntax for constructing a function
;; *without* having to give it a name
(ns fpinclojure.exercises.gettingstarted.anonymous-functions
  (:require
   [fpinclojure.exercises.gettingstarted.my-program :refer [abs factorial format-result]]))

;; Some examples of anonymous functions:
(defn -main [& _]
  (println (format-result "absolute value" -42 abs))
  (println (format-result "factorial" 7 factorial))
  (println (format-result "increment" 7 (fn [x] (inc x))))
  (println (format-result "increment2" 7 #(inc %)))
  (println (format-result "increment3" 7 inc))
  (println (format-result "increment4" 7 (fn [x] (let [r (inc x)] r)))))

(comment
  (with-out-str
    (-main))
  )
