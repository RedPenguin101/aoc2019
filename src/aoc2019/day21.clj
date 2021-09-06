(ns aoc2019.day21
  (:require
   [clojure.test :refer [deftest is are]]
   [clojure.string :as str]
   [clojure.set :as set]
   [aoc2019.intcode :refer [run-program2 parse-program output->screen int->ascii resume]]))

(def program (parse-program (slurp "resources/day21input")))

(output->screen (:outputs (run-program2 program)))

(-> (run-program2 program)
    (assoc :inputs (mapv (clojure.set/map-invert int->ascii) "WALK\n"))
    (assoc :outputs [])
    resume
    :outputs
    output->screen)

(defn x [comp]
  (fn [input]
    (let [comp (resume (assoc comp :inputs (mapv (set/map-invert int->ascii) input)
                              :outputs []))]
      (println (str/join "\n" (output->screen (:outputs comp))))
      (x comp))))

(comment
  (def just-d "OR D J\nWALK\n")
  (def input  "NOT A J\nNOT B T\nOR T J\nNOT C T\nOR T J\nAND D J\nWALK\n")
  (def input1  "NOT A J\nNOT B T\nOR T J\nAND D J\nWALK\n")
  (def input2  "NOT A J\nNOT B T\nOR T J\nNOT C T\nOR T J\nAND D J\nRUN\n")


  ((x (run-program2 program)) input)
  ((x (run-program2 program)) just-d)
  ((x (run-program2 program)) input1)
  ((x (run-program2 program)) input2)


  (resume (assoc (run-program2 program)
                 :inputs (mapv (set/map-invert int->ascii) input)
                 :outputs [])))

(defn hull [num pad]
  (let [n (Integer/toBinaryString num)]
    (apply str (replace {\1 \# \0 \. 1 \# 0 \.} (concat (repeat (- pad (count n)) 0) n)))))

(defn make-hulls [length]
  (map #(hull % length) (range 0 (Math/pow 2 length))))

(defn next-hulls [move hull]
  (remove #{hull} (map #(str (subs hull move) %)
                       (make-hulls move))))

(defn map-decoll [m]
  (reduce-kv (fn [A k v]
               (merge A (into {} (map #(vector % k) v))))
             {}
             m))

".XX. = dead
 #XX. = walk
 .XX# = jump
 "

(defn can-walk? [[a]] (= \# a))
(defn can-jump? [[_ _ _ d]] (= \# d))
(defn must-jump? [xs] (and (not (can-walk? xs))
                           (can-jump? xs)))
(defn must-walk? [xs] (and (can-walk? xs)
                           (not (can-jump? xs))))
(defn cant-jump? [xs] (not (can-jump? xs)))
(defn doomed? [xs] (not (or (can-walk? xs)
                            (can-jump? xs))))

(def four-choice
  {"##.." :walk
   ".###" :jump
   ".##." :doomed
   "#..." :walk
   ".#.." :doomed
   "...." :doomed
   "#..#" :jump
   "##.#" :jump
   "#.##" :jump
   "####" :walk
   "...#" :jump
   ".#.#" :jump
   "..##" :jump
   "###." :walk
   "#.#." :walk
   "..#." :doomed})

(defn walk [hull] (when (can-walk? hull) (subs hull 1)))

(defn jump [hull]
  (if (< (count hull) 8) (throw (ex-info "tried to jump with hull < 8" {:hull hull}))
      (when (can-jump? hull) (subs hull 4))))

(defn action [hull]
  (cond
    (= 4 (count hull)) (four-choice hull)
    (doomed? hull) :doomed

    (< (count hull) 8)
    (cond (must-jump? hull) :jump
          (and (can-jump? hull)  (= :doomed (action (walk hull)))) :jump
          (and (cant-jump? hull) (= :doomed (action (walk hull)))) :doomed
          :else :walk)

    (and (= :doomed (action (walk hull)))
         (= :doomed (action (jump hull)))) :doomed

    (= :doomed (action (walk hull))) :jump
    (= :doomed (action (jump hull))) :walk

    :else :walk))


(comment
  (let [h "#..##..."]
    {:action (action h)
     :can-walk (can-walk? h)
     :must-walk (must-walk? h)
     :on-walk (action (walk h))
     :can-jump (can-jump? h)
     :must-jump (must-jump? h)
     :on-jump (action (jump h))}))

(deftest t
  (are [hull ac] (= ac (action hull))
    "#...#" :walk
    "#..#." :jump
    "..###" :jump
    "#.#..###" :doomed
    ".########" :jump
    "##.#..###" :jump))

(defn try-rules [hull]
  (let [a (action hull)]
    (case a
      :jump (cond
              (cant-jump? hull) :fail
              (<= (count hull) 8) (subs hull 4)
              :else (recur (jump hull)))
      :walk (cond
              (not (can-walk? hull)) :fail
              (<= (count hull) 4) (subs hull 1)
              :else (recur (walk hull)))
      :doomed :doomed)))

(defn tile-freqs [hulls]
  (into {} (map vector
                "abcdefghi"
                (map frequencies (apply map vector hulls)))))

(comment
  (defn t ([n] #(= \# (nth % n))))

  (do
    (def a (t 0))
    (def not-a (complement a))
    (def b (t 1))
    (def not-b (complement b))
    (def c (t 2))
    (def not-c (complement c))
    (def d (t 3))
    (def not-d (complement d))
    (def e (t 4))
    (def not-e (complement e))
    (def f (t 5))
    (def not-f (complement f))
    (def g (t 6))
    (def not-g (complement g))
    (def h (t 7))
    (def not-h (complement h))
    (def i (t 8))
    (def not-i (complement i)))

  ;; (x->b), (x->b), (x->b)
  (defn or-p
    ([p1 p2] (fn [x] (or (p1 x) (p2 x))))
    ([p1 p2 & rst]
     (apply or-p (or-p p1 p2) rst)))

  (defn and-p
    ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
    ([p1 p2 & rst]
     (apply and-p (and-p p1 p2) rst)))

  (def test-expr2
    (or-p not-a
          (and-p a d h (or e not-c))))

  (def input3 ;; ~A | A&D&H&E
    "OR A J
   AND D J
   AND H J
   AND E J
   NOT A T
   OR T J 
   RUN\n")

  ;  v   v
  "#####.#..########"
  ;   AbcDefgHi
  ;; -> not c

;     v   v   v
  "#####.##.#.#..###"
  ;    AbCDefgHi
  ;; -> not b


  (def input4 ;; ~A | A&D&H&(~B|~C)
    "NOT C J
     NOT B T
     OR T J
     AND A J
     AND D J
     AND H J
     NOT A T
     OR T J 
     RUN\n")

  ((x (run-program2 program)) input4)

  (resume (assoc (run-program2 program)
                 :inputs (mapv (set/map-invert int->ascii) input4)
                 :outputs [])))
