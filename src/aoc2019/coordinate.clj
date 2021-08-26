(ns aoc2019.coordinate
  (:require [clojure.test :refer [deftest is]]))

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

(defn print-grid [grid]
  (remove nil? (doall (map println grid))))

(deftest t
  (is (= [[1 2 3] [4 5 6] [7 8 9]] (de-coordinate (coordinate [[1 2 3] [4 5 6] [7 8 9]])))))