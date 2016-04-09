(ns clj.core
  (:gen-class))


(defn full [functions function-args terminals level maxlevels]
  "Creates a program tree that is a full tree (all terminal leaves are on the same level)."
  ;gets a random index in the function vector
  (let [rnd-func-idx (rand-int (count functions))]
    (if (not= level maxlevels)
      ;preappend the randomly picked function to a list of arguments
      (cons (nth functions rnd-func-idx) (loop [args 0 arg-count (nth function-args rnd-func-idx) arg-list '()] 
        (if (not= args arg-count)
          ;make a full tree of one level smaller and preappend that to the argument list
          (recur (+ 1 args) arg-count (cons (full functions function-args terminals (+ 1 level) maxlevels) arg-list))
           arg-list)))
      ;return one of the terminals at the bottom level
      (nth terminals (rand-int (count terminals))))))

(defn grow [functions function-args terminals level maxlevels]
  "Creates a programm tree by 'growing it' from the root and has randomness to the breadth and depth"
  ;if anot at bottom, grow
  (if (not= level maxlevels)
    ;if the root level or if a random int is above 2 (70% chance), must be a function
    (if (or (= level 1) (> (rand-int 9) 2))
      (let [rnd-func-idx (rand-int (count functions))]
        ;preappend the chosen function to the arguemnt list
        (cons (nth functions rnd-func-idx) (loop [args 0 arg-count (nth function-args rnd-func-idx) arg-list '()]
          (if (not= args arg-count)
            ;generate the argument list by recurively calling grow
            (recur (+ 1 args) arg-count (cons (grow functions function-args terminals (+ 1 level) maxlevels) arg-list))
            arg-list))))
      (nth terminals (rand-int (count terminals))))
    (nth terminals (rand-int (count terminals)))))





(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [functions '[+ - *] function-args '[2 2 2] terminals '[1 2 3 4 5 6 7 8 9]]
        (let [fun (grow functions function-args terminals 1 6)]
          (prn fun)
          (prn (eval fun)))))

