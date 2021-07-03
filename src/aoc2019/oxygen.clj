(ns aoc2019.oxygen
  (:require [clojure.test :refer [deftest is]]
            [aoc2019.intcode :refer [parse-program boot2 resume]]
            [clojure.set :refer [intersection union difference]]))

"program loop
 * accept a movement command
 * move
 * report status
 
 Move commands: 1234 = NSWE
 status codes:
 0: wall, position not changed
 1: moved
 2: moved, current pos is oxy system
 
 What is the fewest number of movement commands"

(def program (parse-program (slurp "resources/day15input")))

(def start-robot {:position [0 0] :computer (boot2 program)})

(defn move [[x y] command]
  (case command
    1 [x (inc y)]
    2 [x (dec y)]
    3 [(dec x) y]
    4 [(inc x) y]))

(defn move-robot [robot dir]
  (let [new-comp (resume (:computer robot) dir)
        status (first (:outputs new-comp))]
    (case status
      0 (-> robot (assoc :computer new-comp) (assoc :last-action :hit-wall))
      1 (-> robot (update :position move dir) (assoc :computer new-comp) (assoc :last-action :moved))
      2 (-> robot (update :position move dir) (assoc :computer new-comp) (assoc :last-action :found-oxy)))))

(defn new-paths [path robot moves]
  (for [move moves
        :let [new-robot (move-robot robot move)]
        :when (not= :hit-wall (:last-action new-robot))]
    [(conj path move) new-robot]))

(defn search [paths visited]
  (let [[path robot] (first paths)]
    (cond (= :found-oxy (:last-action robot)) [path (:position robot)]
          (visited (:position robot)) (recur (rest paths) visited)
          :else (recur (concat (rest paths) (new-paths path robot [1 2 3 4]))
                       (conj visited (:position robot))))))

(comment
  (time (count (first (search [[[] start-robot]] #{}))))
  ;; => 248
  (second (search [[[] start-robot]] #{}))
  ;; => [-20 12]
  ;; oxy position
  )

(deftest t
  (is (= 248 (count (first (search [[[] start-robot]] #{}))))))

(comment
  "Part 2
   It takes one minute for oxygen to spread to all open locations that 
   are adjacent to a location that already contains oxygen.
   
   Use the repair droid to get a complete map of the area. How many minutes 
   will it take to fill with oxygen?")

(def reverse-direction {1 2 2 1 3 4 4 3})

(defn possible-moves [robot]
  (remove (fn [dir] (= :hit-wall (:last-action (move-robot robot dir)))) [1 2 3 4]))

(defn find-next-edge
  ([robot dir] (find-next-edge robot dir (:position robot) 0 [(:position robot)]))
  ([robot dir start length path]
   (let [new-robot (move-robot robot dir)
         moves (possible-moves new-robot)]
     (if (= 2 (count moves))
       (recur new-robot
              (first (remove #(= (reverse-direction dir) %) moves))
              start
              (inc length)
              (conj path (:position new-robot)))
       ;; returns an edge [start end length robot path]
       [start (:position new-robot) (inc length) new-robot (conj path (:position new-robot))]))))

(defn known-edge? [edges edge]
  (let [e (map #(take 2 %) edges)
        re (map reverse e)]
    ((set (concat e re))
     (take 2 edge))))

;; pretty sure I'm missing something important with the else on this, feels wrong.
(defn map-space [robot edges [next-edge & other-edges]]
  (let [new-robot (if next-edge (nth next-edge 3) robot)
        new-edges (->> (possible-moves new-robot)
                       (map #(find-next-edge new-robot %))
                       (remove #(known-edge? edges %)))]
    (cond next-edge (recur new-robot (conj edges next-edge) (concat other-edges new-edges))
          (empty? new-edges) edges
          :else     (recur new-robot edges new-edges))))

(comment
  ;; generate the edges - slow!
  (time (def map-edges (map-space start-robot [] [])))

  (count map-edges)
  ;; => 178

  (defn interrogate-edge [edge]
    (let [[srt end dst robot] edge]
      {:edge-end end
       :edge-start srt
       :moves (possible-moves robot)
       :dist dst}))

  (map interrogate-edge (take 2 map-edges))

  ;; from part 1 oxygen is at -20 12. Need to normalize
  [(+ 20 -20) (+ 18 12)]
  ;; => [0 30]

  (count (set (mapcat last map-edges)))
  ;; => 799

  ;; generating the maze - -20/-18 are the min x/y coords, so normalize on that. 
  (let [floors (set (mapcat last map-edges))
        floors (map (fn [[x y]] [(+ 20 x) (+ 18 y)]) floors)
        grid (vec (repeat 39 (vec (repeat 39 "#"))))]
    (->> (assoc-in (reduce (fn [grid [x y]]
                             (assoc-in grid [y x] "_"))
                           grid
                           floors)
                   [0 30] "*")
         (map #(apply str %))))
  ;; => ("_______#_________#___________#*__#_____"
  ;;     "##_###_###_#######_#######_#_#_#_###_#_"
  ;;     "___#_______#___#___#___#___#___#___#_#_"
  ;;     "_###########_#_#_###_###_#_#######_#_#_"
  ;;     "___#_________#_#___#___#_#_#_____#___#_"
  ;;     "_#_#_#########_###_###_#_###_###_#####_"
  ;;     "_#_#___#___#___#___#___#_____#___#_____"
  ;;     "##_###_#_###_#_#_###_#_#######_#####_##"
  ;;     "___#_____#___#_#_____#_______#_____#___"
  ;;     "_###_#####_###########_#########_#_###_"
  ;;     "_#___#___#___________#_#_______#_#___#_"
  ;;     "_#_###_#_###_#_###_###_#_#####_#####_#_"
  ;;     "_____#_#___#_#___#_#___#_____#_#_____#_"
  ;;     "######_###_#_###_###_#######_#_#_#####_"
  ;;     "_______#_#_#___#_____#_____#_#___#___#_"
  ;;     "_#######_#_#######_#####_#_#_#####_###_"
  ;;     "_#_____#_#_#_____#_____#_#_#_#___#_____"
  ;;     "_#_#_#_#_#_#_###_#####_###_#_#_#_#_####"
  ;;     "_#_#_#___#___#_#_____#_____#___#_#_____"
  ;;     "_###_###_#####_#################_#####_"
  ;;     "_____#_#___#___#_____________#___#___#_"
  ;;     "######_###_#_#_###_#########_#_#####_#_"
  ;;     "_________#___#_____#_____#___#_#_____#_"
  ;;     "_#####_#_#############_###_###_###_###_"
  ;;     "___#_#_#___#___________#___#_____#_____"
  ;;     "##_#_#_#####_#####_###_#_###_###_#_####"
  ;;     "___#_#_______#_#___#___#_#_____#_#_____"
  ;;     "_###_#####_###_#_###_###_#####_#_#####_"
  ;;     "___#_#___#_____#_#_____#_____#_#_#_____"
  ;;     "##_#_#_#_#######_#_#########_#_#_###_##"
  ;;     "_#___#_#_________#_#_____#___#_#___#___"
  ;;     "_#_###_###########_#_###_#_#######_####"
  ;;     "_#_#___#_#___#_____#_#_#___#_____#_#___"
  ;;     "_#_#_###_#_#_#_#####_#_#######_#_#_#_#_"
  ;;     "_#_#___#___#_______#_____#___#_#_#___#_"
  ;;     "_#_###_#######_#########_#_#_#_#_#####_"
  ;;     "_#___#___#___#_#_______#_#_#___#_______"
  ;;     "_###_###_#_#_###_#_#####_#_###########_"
  ;;     "_______#___#_____#_________#___________")
  )

(defn adjacents
  [[x y]]
  [[x (inc y)]
   [x (dec y)]
   [(dec x) y]
   [(inc x) y]])

;; who needs connectivity algorithms!
(defn flow [oxy-full not-full it]
  (if (empty? not-full) it
      (let [adj (set (mapcat adjacents oxy-full))
            new-full (intersection adj not-full)]
        (recur (union oxy-full new-full) (difference not-full new-full) (inc it)))))

(comment
  (time (let [floors (set (mapcat last map-edges))
              floors (set (map (fn [[x y]] [(+ 20 x) (+ 18 y)]) floors))
              oxy [0 30]]
          (flow #{oxy} floors 0)))
  ;; => 382
  )


