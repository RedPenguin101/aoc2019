(ns aoc2019.day18-6
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;;;;;;;;;;;;;;;;;;;;
;; Coordinate fns
;;;;;;;;;;;;;;;;;;;;

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

(defn neighbours [[x y]]
  (hash-set [(inc x) y] [(dec x) y]
            [x (inc y)] [x (dec y)]))

;;;;;;;;;;;;;;;;;;;;

(def ex
  "#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################")

(def full-coords (into {} (remove #(= \# (second %)) (coordinate (str/split-lines ex)))))

(defn connected-nodes
  ([coords node] (connected-nodes coords node 1 #{node}))
  ([coords start dist seen]
   (mapcat (fn [coord]
             (let [v (coords coord)]
               (cond
                 (= \. v) (connected-nodes coords coord (inc dist) (conj seen coord))
                 :else [[coord dist]])))
           (filter coords (set/difference (neighbours start) seen)))))

(connected-nodes full-coords [8 4])

(defn bfs-adj-matr [coords start visited]
  (let [cn (connected-nodes coords start)]
    ()))

;; @{} -> e{e}
;; @{} -> c{c} etc.
;; c{c} -> e{ce}
;; c{c} -> h{ch}
;; h{ch} -> m{chm}

(def char->int
  (zipmap (str/join (cons "@" ((juxt identity str/upper-case) "abcdefghijklmnopqrstuvwxyz")))
          (range)))

