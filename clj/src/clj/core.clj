(ns clj.core
  (:gen-class))


(defn full [functions function-args terminals level maxlevels]
  "Creates a program tree that is a full tree (all terminal leaves are on the same level)."
  (let [rnd-func-idx (rand-int (count functions))]
    (if (not= level maxlevels)
      (cons (nth functions rnd-func-idx) (loop [args 0 arg-count (nth function-args rnd-func-idx) arg-list '()] 
        (if (not= args arg-count)
          (recur (+ 1 args) arg-count (cons (full functions function-args terminals (+ 1 level) maxlevels) arg-list))
           arg-list)))
      (nth terminals (rand-int (count terminals))))))





(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [functions '[+ - *] function-args '[2 2 2] terminals '[1 2 3 4 5 6 7 8 9]]
        (let [fun (full functions function-args terminals 1 6)]
          (prn fun)
          (prn (eval fun)))))

