(ns clj.core
  (:gen-class
:name Core ))


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
    ;if the root level or if a random int is above 1 (80% chance), must be a function
    (if (or (= level 1) (> (rand-int 9) 1))
      (let [rnd-func-idx (rand-int (count functions))]
        ;preappend the chosen function to the arguemnt list
        (cons (nth functions rnd-func-idx) (loop [args 0 arg-count (nth function-args rnd-func-idx) arg-list '()]
          (if (not= args arg-count)
            ;generate the argument list by recurively calling grow
            (recur (+ 1 args) arg-count (cons (grow functions function-args terminals (+ 1 level) maxlevels) arg-list))
            arg-list))))
      (nth terminals (rand-int (count terminals))))
    (nth terminals (rand-int (count terminals)))))
    
(defn rhah [functions function-args terminals population-size max-tree-size]
"creates an initial generation using 'ramped half and half'.  Ramped refers to how it starts with small trees and increases the tree sizes and half and half refers to using grow and full equally"
  (let [programs-per-size (/ population-size (- max-tree-size 1))] ;divide populationsize by number of possible sizes for number of programs of that size per tree
    (loop [size 2 program-list '[]] ;loop while size is less than max
      (if (<= size max-tree-size)
        (recur (+ size 1) (into [] (concat (loop [num-this-size 1 inner-list program-list] ;loop for (pop-size/max-tree-size) percent of the total population
          (if (<= num-this-size programs-per-size)
            (recur (+ num-this-size 1) (cons (if (= (mod num-this-size 2) 0) ;preappend either a grow or a full tree to the list of programs
              (grow functions function-args terminals 1 size)
              (full functions function-args terminals 1 size)) inner-list))
            inner-list)) program-list))) ;concat the list of programs generated for that size with the rest of the generated programs
        program-list)))) 
        
(defrecord ProgramFitness [program raw standardized adjusted normalized])

(defn gen-zero [functions function-args terminals population-size max-tree-size]
"creates a vector of program-fitness structures and fills the program field with programs generated from rhah"
  (into [] (map #(ProgramFitness. % nil nil nil nil) (rhah functions function-args terminals population-size max-tree-size) ))) 
  
(defn raw-fitness [programs fitness-function] 
"evaluates the raw fitness of each program using the fitness function"
  (into [] (map #(assoc % :raw (fitness-function (get % :program))) programs)))

(defn stdfitness [programs bestValue]
  "calculates standard fitness, closer to zero is better. bestvalue is the best possible score the 
  fitness function can give.  If lower scores are better, use 0. if the highest score is unknown, 
  use an arbitrarily high value"
  (into [] (map #(assoc % :standardized (Math/abs (- bestValue (get % :raw)))) programs)))

(defn adjusted-fitness [programs]
  "fills in the adjusted fitness of programs. adjusted fitness is 1/(1+standard)"
  (into [] (map #(assoc % :adjusted (/ 1 (+ 1 (get % :standardized)))) programs)))

(defn normalized-fitness [programs]
  "fills in the normalized fitness of programs.  normalized fitness = adjusted/(sum of all adjusted)"
  (let [sum (reduce + 0 (map #(get % :adjusted) programs))]
    (into [] (map #(assoc % :normalized (/ (get % :adjusted) sum))) programs)))

(defn run-fitness [programs fitness-function best-value]
  "Runs all fitness calculations and sorts them"
  (let [fitnesses (sort-by :normalized > (normalized-fitness (adjusted-fitness (stdfitness (raw-fitness programs fitness-function) best-value))))] fitnesses)) ;run all fitness functions and sort them

(defn pick-individual [progs]
  "picks a random individual from the program list"
  (let [random (rand 1.0)]
    (nth progs (loop [i 0 rnd (- random (get (first progs) :normalized))]
      (if (>= rnd 0)
        (recur (+ i 1) (- rnd (get (nth progs (+ i 1)) :normalized)))
        i)))))
      
(defn height [tree]
  "returns the height of the tree at its deepest"
  (if-let [sub-trees (seq (filter coll? tree))]
    (inc (apply max (map height sub-trees)))
    1))
  

(defn get-cross-point [lst keep-short]
  (let [keep-short (if (> 0 keep-short) 2 1)]
   (loop [queue clojure.lang.PersistentQueue/EMPTY n 1 winner lst]
     (if (= n 1) 
       (recur (reduce conj queue (rest lst)) (+ n 1) lst)
       (if (not (empty? queue)) 
                (let [tree (peek queue) queue2 (pop queue)]
                  (recur (if (seq? tree) (reduce conj queue2 (rest tree)) queue2) (+ n 1) (if (< (rand-int (- (* (* 10 n) keep-short) 1)) (if (seq? tree) 9 1)) tree winner)))
                winner)))))

(defn set-subtree [new-subtree old-subtree program]
  "takes a program and two subtrees. one of them is what you are replacing and one is what you are replacing with"
  (seq (map #(if (= % old-subtree)
    new-subtree
    (if (seq? %)
      (set-subtree new-subtree old-subtree %)
      %)) program)))

(defn crossover [parent1 parent2]
  "takes 2 parents and performs crossover on them, yeilding 2 new children, which will be returned in a vector of 2 elements"
  (let [too-tall (- (+ (height parent1) (height parent2)) 20)];if the sum of the heights are above 20, too-tall wil be > 0, and get-rand will make it more likely to pick from the top
    (let [cross1 (get-cross-point parent1 too-tall) cross2 (get-cross-point parent2 too-tall)]
      [(set-subtree cross2 cross1 parent1) (set-subtree cross1 cross2 parent2)])))

(defn mutate [program functions function-args terminals]
  "does a point mutation on the parent"
  (let [mutate-subtree (get-cross-point program) mutate-size (+ (rand-int 8) 1)]
    (map #(if (= % mutate-subtree)
    (if (= (rand-int 1) 0)
      (full functions function-args terminals 1 mutate-size)
      (grow functions function-args terminals 1 mutate-size))
    (if (seq? %)
      (mutate %)
      %)) program)))
    
(defn nextGen [programs]
  (let [crossover-times (Math/floor (* (count programs) 0.9)) total (count programs)];90% of time crossover, else replicate
    (loop [n 0 lst []]
      (if (< n total) 
        (if (< n crossover-times)
          (let [p1 (get (pick-individual programs) :program) p2 (get (pick-individual programs) :program)]
            (let [crossed (crossover p1 p2)]
              (recur (+ n 2) (concat lst [(ProgramFitness. (first crossed) nil nil nil nil) (ProgramFitness. (first (rest crossed)) nil nil nil nil)]))))
          (recur (+ n 1) (cons (pick-individual programs) lst)))
        (into [] lst)))))



      

(defn just-do-it [funs argmap terminals pop-size max-depth fit-func best-value gens]
  (prn (loop [gen (gen-zero funs argmap terminals pop-size max-depth) best-of-run nil  gens-completed 0]
   (prn gens-completed)
    (if (< gens-completed gens)
      (let [gen (run-fitness gen fit-func best-value)]
        (let [best-of-gen (first gen)]
          (if (= best-value (get best-of-gen :raw))
                 best-of-gen
                 (recur (nextGen gen) (if (= gens-completed 0)
                                             best-of-gen
                                             (if (< (get best-of-run :standardized) (get best-of-gen :standardized))
                                                    best-of-gen
                                                    best-of-run)) (+ gens-completed 1))))) best-of-run))))


(defn -main
"I don't do a whole lot ... yet."
  [& args]
  (just-do-it '[+ - * FitnessCart/safeDiv FitnessCart/gt FitnessCart/abs] '[2 2 2 2 2 1] '[x v -1] 50 6 #(FitnessCart/fitness %) 0 20))

;  (FitnessCart/go))
;  (let [functions '[+ - * FitnessTest/safeDiv]  function-args '[2 2 2 2] terminals '[x 1 2 3 4 5 6 7 8 9]]
;    (just-do-it functions function-args terminals 50 6 #(FitnessTest/fitness %) 2000 20)))

