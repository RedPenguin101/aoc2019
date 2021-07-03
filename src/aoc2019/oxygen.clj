(ns aoc2019.oxygen
  (:require [clojure.test :refer [deftest is]]
            [aoc2019.intcode :refer [parse-program boot2 resume2]]
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

(defn move [[x y] command]
  (case command
    1 [x (inc y)]
    2 [x (dec y)]
    3 [(dec x) y]
    4 [(inc x) y]))

(defn possible-moves [robot]
  (if (= :found-oxy robot) [1 2 3 4]
      (keep #(when (not ((second robot) (move (first robot) %))) %) [1 2 3 4])))

(comment
  (possible-moves [[0 0] #{}])
  (possible-moves [[0 0] #{[1 1]}])
  (possible-moves [[0 0] #{[0 1]}]))

(defn run [computer robot input]
  (let [new-comp (resume2 computer input)
        status (first (:outputs new-comp))]
    (when (= status 2) (println "found!" (move (first robot) input)))
    (cond (= status 2) [new-comp :found-oxy]
          (= status 0) [new-comp [(first robot) (conj (second robot) (move (first robot) input))]]
          :else [new-comp [(move (first robot) input) (second robot)]])))

(defn new-paths [path computer robot moves]
  (for [move moves]
    (into [(vec (conj path move))]
          (run computer robot move))))

(defn search [paths it visited]
  (let [[path computer robot] (first paths)
        moves (possible-moves robot)]
    (cond (> it 100000) [:break path]
          (= :found-oxy robot) path
          (visited (first robot)) (recur (rest paths) (inc it) visited)
          :else (recur (concat (rest paths)
                               (new-paths path computer robot moves))
                       (inc it)
                       (conj visited (first robot))))))

(comment
  (new-paths [1 2 3] (boot2 program) [[0 0] #{}] [1 2 3 4])

  (time (count (search [[[] (boot2 program) [[0 0] #{}]]] 0 #{})))
  ;; => 248

  ;; oxy is at [-20 12]
  )

#_(deftest t
    (let [program (parse-program (slurp "resources/day15input"))]
      (is (= 248 (count (search [[[] (boot2 program) [[0 0] #{}]]] 0 #{}))))))

(comment
  "Part 2
   It takes one minute for oxygen to spread to all open locations that 
   are adjacent to a location that already contains oxygen.
   
   Use the repair droid to get a complete map of the area. How many minutes 
   will it take to fill with oxygen?")

(def start-robot {:position [0 0] :computer (boot2 program)})

(defn possible-moves2 [robot]
  (filter (fn [dir] (not= 0 (first (:outputs (resume2 (:computer robot) dir))))) [1 2 3 4]))

(defn move2 [robot dir]
  (-> robot
      (update :position move dir)
      (update :computer resume2 dir)))

(comment
  (possible-moves2 start-robot)

  (move2 start-robot 3))

(def reverse-direction {1 2 2 1 3 4 4 3})

(defn find-next-edge
  ([robot dir] (find-next-edge robot dir (:position robot) 0 [(:position robot)]))
  ([robot dir start length path]
   (let [new-robot (move2 robot dir)
         moves (set (possible-moves2 new-robot))]
     (if (= 2 (count moves))
       (recur new-robot
              (first (remove #(= (reverse-direction dir) %) moves))
              start
              (inc length)
              (conj path (:position new-robot)))
       [start (:position new-robot) (inc length) new-robot (conj path (:position new-robot))]))))

(comment
  (possible-moves2 start-robot)
  (find-next-edge start-robot 3 [0 0] 0 []))

(defn have-edge? [edges edge]
  (let [e (map #(take 2 %) edges)
        re (map reverse e)]
    ((set (concat e re))
     (take 2 edge))))

(defn map-space [robot edges [nxt-edge & rst] it]
  (cond (> it 1000) :break
        nxt-edge
        (recur (nth nxt-edge 3)
               (conj edges nxt-edge)
               (concat rst (remove #(have-edge? edges %)
                                   (map #(find-next-edge (nth nxt-edge 3) %
                                                         (:position (nth nxt-edge 3)) 0
                                                         [(:position (nth nxt-edge 3))])
                                        (possible-moves2 (nth nxt-edge 3)))))
               (inc it))
        :else (let [new-edges (remove #(have-edge? edges %)
                                      (map #(find-next-edge robot % (:position robot) 0 [(:position robot)])
                                           (possible-moves2 robot)))]
                (if (empty? new-edges)
                  edges
                  (recur robot edges new-edges (inc it))))))

(comment
  ;; generate the edges - slow!
  (time (def map-edges (map-space start-robot [] [] 0)))

  (count map-edges)
  ;; => 178

  (defn interrogate-edge [edge]
    (let [[srt end dst robot] edge]
      {:edge-end end
       :edge-start srt
       :moves (possible-moves2 robot)
       :dist dst}))

  (map interrogate-edge map-edges)

  ;; from part 1 oxygen is at -20 12
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


