(ns aoc2019.day17
  (:require [aoc2019.intcode :refer [parse-program resume boot2]]
            [clojure.set :as set]))

(def program (parse-program (slurp "resources/day17input")))

(comment
  "small robots that, unaware of the impending danger, are now trapped on exterior scaffolding
   some wired cameras and a small vacuum robot currently asleep at its charging station
   Running the ASCII program on your Intcode computer will provide the current view of the scaffolds."

  (->> (:outputs (resume (boot2 program)))
       (map {35 \# 46 \. 10 \newline})
       (apply str)
       (clojure.string/split-lines))

  "# represents a scaffold and . represents open space
   vacuum robot is visible as ^, v, <, or > on a scaffold
   X if it falls off
   
   Locate all scaffold intersections;
   distance between its left edge and the left edge of the view multiplied by the 
   distance between its top edge and the top edge of the view.
   (* x y) zero indexed
   What is the sum of the alignment parameters
   ")

(def int->ascii {10 \newline 35 \# 44 \, 46 \.
                 48 \0 49 \1 50 \2 51 \3 52 \4 53 \5 54 \6 55 \7 56 \8 57 \9
                 58 \: 60 \< 62 \>
                 65 \A 66 \B 67 \C 68 \D 69 \E 70 \F 71 \G 72 \H 73 \I 74 \J 75 \K 76 \L 77 \M 78 \N 79 \O
                 80 \P 81 \Q 82 \R 83 \S 84 \T 85 \U 86 \V 87 \W 88 \X 89 \Y 90 \Z
                 94 \^ 97 \a 105 \i 110 \n 118 \v})

(def ascii->int (set/map-invert int->ascii))

(defn output->screen [output]
  (->> output
       (map int->ascii)
       (apply str)
       (clojure.string/split-lines)))

(defn coordinate
  "Turns a 2d grid (coll of coll of x) into a map of coord->x"
  [grid]
  (into {} (apply concat (map-indexed (fn [y x-row] (map-indexed (fn [x v] [[x y] v]) x-row)) grid))))

(defn adjacents [[x y]] #{[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]})

(comment
  (def view (output->screen (:outputs (resume (boot2 program)))))

  (def scaffolds (reduce-kv (fn [s coord v] (if (= v \#) (conj s coord) s)) #{} (coordinate view)))

  (->> scaffolds
       (filter (fn [coord] (> (count (set/intersection (adjacents coord) scaffolds)) 2)))
       (map #(apply * %))
       (apply +))
  ;; => 2788
  )

(comment
  "Part2: Make the vacuum robot visit every part of the scaffold at least once.
   
   Force the vacuum robot to wake up by changing the value in your ASCII program at address 0 from 1 to 2.
   
   Provide movement rules
   1. main movement routine: movement functions A B C, separate with comma (44) end with newline (10)
   2. define movement functions: L R, number for move fwd spaces. e.g. '10,L,8,R,6' (max 20)
   3. set continuous feed y/n
   
   Once it finishes the programmed set of movements, robot will output a large, non-ASCII value
   
   After visiting every part of the scaffold at least once, what value does it output?")

(defn separate-ascii [chars next]
  (let [nums (set "1234567890")]
    (if (or (empty? chars) (and (nums next) (nums (last chars))))
      (conj chars next)
      (conj chars \, next))))

(comment
  "The screen"
  (drop-last 2 (output->screen (:outputs (resume (boot2 (assoc program 0 2))))))

  (set (:outputs (resume (boot2 (assoc program 0 2)))))

  "Just eyeballed it and fiddled with the permutations"
  '[(A) R12L8L4L4
    (B) L8R6L6
    (A) R12L8L4L4
    (B) L8R6L6
    (C) L8L4R12L6L4
    (A) R12L8L4L4
    (C) L8L4R12L6L4
    (A) R12L8L4L4
    (C) L8L4R12L6L4
    (B) L8R6L6]

  (def move-fns {\A "R12L8L4L4"
                 \B "L8R6L6"
                 \C "L8L4R12L6L4"})

  (def routine "ABABCACACB")

  (apply str (mapcat move-fns routine))
  (reduce separate-ascii [] routine)
  (map #(reduce separate-ascii [] %) (vals move-fns))

  (def inputs
    (mapv ascii->int
          (concat (conj (reduce separate-ascii [] routine) \newline)
                  (mapcat #(conj (reduce separate-ascii [] %) \newline) (vals move-fns))
                  [\n \newline])))

  (last (:outputs (resume (assoc (resume (boot2 (assoc program 0 2))) :outputs [] :inputs inputs)))))