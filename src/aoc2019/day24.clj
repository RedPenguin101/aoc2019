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

(defn left-side [cells]   (filter #(= 0 (first %)) cells))
(defn right-side [cells]  (filter #(= 4 (first %)) cells))
(defn top-side [cells]    (filter #(= 0 (second %)) cells))
(defn bottom-side [cells] (filter #(= 4 (second %)) cells))

(defn neighbours-multi [grids level [x y :as cell]]
  (+ (neighbours cell (grids level))
     (count (case cell
              [2 1] (top-side (grids (dec level)))
              [2 3] (bottom-side (grids (dec level)))
              [1 2] (left-side (grids (dec level)))
              [3 2] (right-side (grids (dec level)))
              #{}))
     (count (set/intersection
             (grids (inc level))
             (set [(when (= x 0) [1 2])
                   (when (= x 4) [3 2])
                   (when (= y 0) [2 1])
                   (when (= y 4) [2 3])])))))

(def all-coords
  (set (for [x (range 0 5)
             y (range 0 5)
             :when (not= x y 2)]
         [x y])))

(defn step-1-level [grids level]
  (let [living (or (grids level) #{})]
    (->> all-coords
         (keep #(case (neighbours-multi grids level %)
                  1 %
                  2 (when-not (living %) %)
                  nil))
         (set))))

(defn new-lvls [lvls]
  (let [x (sort lvls) min (first x) max (last x)]
    (range (dec min) (+ 2 max))))

(defn step-all-levels [grids]
  (into {} (remove
            (comp empty? second)
            (let [nl (new-lvls (keys grids))
                  new-grids (assoc grids
                                   (last nl) #{}
                                   (first nl) #{})]
              (for [lvl nl]
                [lvl (step-1-level new-grids lvl)])))))

(comment
  (step-all-levels {0 (start-state example)})

  (last (take 11 (iterate step-all-levels {0 (start-state example)})))

  (apply + (map count (vals (last (take 11 (iterate step-all-levels {0 (start-state example)}))))))

  (sort (last (take 7 (iterate step-all-levels {0 (start-state input)}))))

  (apply + (map count (vals (last (take 201 (iterate step-all-levels {0 (start-state input)})))))))
