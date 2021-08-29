(ns aoc2019.day24
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input "#.#.#\n.#...\n...#.\n.###.\n###.#")
(def example "....#\n#..#.\n#..##\n..#..\n#....")

"Simple GOL Based on adjacents (LRUD)
 
    0 1 2 3 4
 D  D L L D D
 L  D L D D D

 Find a loop. What is biodiversity rating of
 that loop

 biodiversity rating of [x y] = 2^(5x + y)
"

;; Coordination utils

(defn coordinate
  "Turns a 2d grid (coll of coll of x) into a map of coord->x"
  [grid]
  (into {} (apply concat (map-indexed (fn [y x-row] (map-indexed (fn [x v] [[x y] v]) x-row)) grid))))

(defn de-coordinate
  "Turns a map of coord->x into a grid (coll of coll of x)"
  ([coord->x] (de-coordinate coord->x nil))
  ([coord->x nil-replace]
   (let [[xs ys] (apply map vector (keys coord->x))
         max-x (apply max xs) max-y (apply max ys)
         grid (vec (repeat (inc max-y) (vec (repeat (inc max-x) nil-replace))))]
     (reduce-kv (fn [g [x y] val] (assoc-in g [y x] val))
                grid coord->x))))

(defn print-grid [coords]
  (->> (de-coordinate (into {} (map vector coords (repeat \#))) \.)
       (map #(apply str %))
       (str/join \newline)
       (println)))

;; 

(defn valid-coord [coord] (every? #(<= 0 % 4) coord))

(defn adjacents [[x y]]
  (set (filter valid-coord
               [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])))

(defn neighbours [cell other-cells]
  (count (set/intersection (adjacents cell) other-cells)))

(defn start-state [input] (set (keep #(when (= \# (second %)) (first %)) (coordinate (str/split-lines input)))))

(defn step [current-cells]
  (->> current-cells
       (mapcat adjacents)
       (set)
       (keep #(case (neighbours % current-cells)
                1 %
                2 (when-not (current-cells %) %)
                nil))
       (set)))

(defn find-loop [state seen]
  (if (seen state) state
      (recur (step state) (conj seen state))))

(defn bio-rate [state]
  (reduce (fn [sum [x y]]
            (+ sum (int (Math/pow 2 (+ x (* 5 y))))))
          0
          state))

(comment
  (-> example
      start-state
      step step step
      print-grid)
  (find-loop (start-state example) #{})
  (bio-rate (find-loop (start-state example) #{}))
  (bio-rate (find-loop (start-state input) #{})))



