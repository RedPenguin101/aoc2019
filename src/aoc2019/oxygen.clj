(ns aoc2019.oxygen
  (:require [clojure.test :refer [deftest is]]
            [aoc2019.intcode :refer [parse-program boot2 resume]]
            [clojure.set :refer [intersection union difference]]))

(def program (parse-program (slurp "resources/day15input")))

;; Robot execution

;; A Robot is its current position, it's onboard computer and it's last status
(def start-robot {:position [0 0]
                  :computer (boot2 program)
                  :last-status :none})

(defn move [[x y] command]
  (case command
    1 [x (inc y)]
    2 [x (dec y)]
    3 [(dec x) y]
    4 [(inc x) y]))

(defn move-robot
  "Returns the state of the robot after moving it in the given direction command.
   If the robot hits a wall, it will be in the same position it started."
  [robot dir]
  (let [new-comp (resume (:computer robot) dir)
        status   (first (:outputs new-comp))]
    (case status
      0 (-> robot (assoc  :computer new-comp) (assoc :last-status :hit-wall))
      1 (-> robot (update :position move dir) (assoc :computer new-comp) (assoc :last-status :moved))
      2 (-> robot (update :position move dir) (assoc :computer new-comp) (assoc :last-status :found-oxy)))))

;; Part 1: Search

(comment "What is the fewest number of movement commands to reach the oxy source?")

(defn new-routes [previous-moves robot moves]
  (for [move moves
        :let [new-robot (move-robot robot move)]
        :when (not= :hit-wall (:last-status new-robot))]
    [(conj previous-moves move) new-robot]))

;; A path is a tuple of [previous-commands robot-state-after-those-commands]
(defn search [paths visited]
  (let [[previous-moves robot] (first paths)]
    (cond (= :found-oxy (:last-status robot)) [previous-moves (:position robot)]
          (visited (:position robot)) (recur (rest paths) visited)
          :else (recur (concat (rest paths) (new-routes previous-moves robot [1 2 3 4]))
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

;; Part 2: Graph

(comment
  "Part 2
   It takes one minute for oxygen to spread to all open locations that 
   are adjacent to a location that already contains oxygen.
   
   Use the repair droid to get a complete map of the area. How many minutes 
   will it take to fill with oxygen?")

(defn possible-moves [robot]
  (remove #(= :hit-wall (:last-status (move-robot robot %))) [1 2 3 4]))

(defn find-next-edge
  "Given an robot and a direction, the robot will follow the maze in that direction until
   it hits a dead end (i.e. has 1 possible move) or a decision point (i.e. has more than 2)
   possible moves. When it finds the decision point it returns an edge: a tuple of
      start-coord 
      end-coord 
      length-of-walk 
      robot-state-at-end 
      path-of-coords-walked"
  ([robot dir] (find-next-edge robot dir (:position robot) 0 [(:position robot)]))
  ([robot dir start length path]
   (let [new-robot (move-robot robot dir)
         moves (possible-moves new-robot)
         reverse-direction {1 2 2 1 3 4 4 3}]
     (if (= 2 (count moves))
       (recur new-robot
              (first (remove #(= (reverse-direction dir) %) moves))
              start (inc length) (conj path (:position new-robot)))
       [start (:position new-robot) (inc length) new-robot (conj path (:position new-robot))]))))

(defn known-edge? [edges edge]
  (let [e (map #(take 2 %) edges)
        re (map reverse e)]
    ((set (concat e re))
     (take 2 edge))))

;; pretty sure I'm missing something important with the else on this, feels wrong.
(defn map-space "returns every edge in the maze"
  [robot edges [next-edge & other-edges]]
  (let [new-robot (if next-edge (nth next-edge 3) robot)
        new-edges (->> (possible-moves new-robot)
                       (map #(find-next-edge new-robot %))
                       (remove #(known-edge? edges %)))]
    (cond next-edge (recur new-robot (conj edges next-edge) (concat other-edges new-edges))
          (empty? new-edges) edges
          :else     (recur new-robot edges new-edges))))

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
  ;; #=wall, _=floor, *=oxy-source
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


  (time (let [floors (set (mapcat last map-edges))
              floors (set (map (fn [[x y]] [(+ 20 x) (+ 18 y)]) floors))
              oxy [0 30]]
          (flow #{oxy} floors 0)))
  ;; => 382
  )
