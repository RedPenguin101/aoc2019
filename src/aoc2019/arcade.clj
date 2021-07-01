(ns aoc2019.arcade
  (:require [aoc2019.intcode :refer [resume run-program2]]
            [clojure.string :as str]))

"Day 13: Care Package
 The arcade cabinet runs Intcode software
 screen capable of drawing square tiles
 every three output instructions specify 
 * x position (distance from the left)
 * y position (distance from the top)
 * tile id (0=empty, 1=wall, 2=block, 3=paddle, 4=ball).
 
 How many block tiles are on the screen when the game exits?"

(defn parse-program [string] (mapv #(Long/parseLong %) (str/split (str/trim-newline string) #",")))

(def program (parse-program (slurp "resources/day13input")))

(comment
  (def st (atom {}))
  (def output (:outputs (run-program2 program)))
  (apply max (set (map second (partition 3 output))))
  (apply max (set (map first (partition 3 output))))
  (* 38 21)
  ;; => 798

  (count (partition 3 output))
  ;; => 798

  (count (filter (fn [[_ _ tile]] (= tile 2)) (partition 3 output)))
  ;; => 320
  )

"Part 2
 
 Memory address 0 represents the number of quarters that have been inserted; set it to 2 to play for free.
 position of the joystick with input instructions: 0=neutral, -1=left, 1=right
 
 Segment display with score: [-1,0,score]"

(defn update-game-state [game-state [x y z]] (assoc game-state [x y] z))

(defn gs->screen [game-state] (sort-by (juxt second first) (map (fn [[[x y] z]] [x y z]) game-state)))

(defn screen->grid [screen]
  (let [display {0 " " 1 "|" 2 "#" 3 "_" 4 "*"}]
    (->> screen
         (remove #(= -1 (first %)))
         (map (comp display last))
         (partition 38)
         (map #(apply str %)))))

(defn game-summary [screen]
  {:grid (screen->grid screen)
   :score (some (fn [[x _ z]] (when (= x -1) z)) screen)
   :blocks (count (filter (fn [[_ _ z]] (= z 2)) screen))
   :ball-position (some (fn [[x y z]] (when (= z 4) [x y])) screen)
   :paddle-position (some (fn [[x y z]] (when (= z 3) [x y])) screen)})

(comment
  (game-summary (gs->screen (reduce update-game-state {} (partition 3 (:outputs (run-program2 program)))))))

(defn move-to-ball [game-summary]
  (let [[ball-x]   (:ball-position game-summary)
        [paddle-x] (:paddle-position game-summary)]
    (cond (= ball-x paddle-x) 0
          (< ball-x paddle-x) -1
          (> ball-x paddle-x) 1)))

(defn tick [game-state computer move-rule its]
  (let [new-gs (reduce update-game-state game-state (partition 3 (:outputs computer)))
        summary (game-summary (gs->screen new-gs))]
    (cond (> its 5000) new-gs
          (:halted computer) new-gs
          (zero? (:blocks summary)) new-gs
          :else (recur new-gs
                       (resume (assoc computer :inputs [(move-rule summary)] :outputs []))
                       move-rule (inc its)))))

(comment
  (let [comp (run-program2 (assoc program 0 2))
        game-state (reduce update-game-state {} (partition 3 (:outputs comp)))]
    (-> (tick game-state comp move-to-ball 0)
        gs->screen
        game-summary))

  (game-summary (gs->screen @st))
  (screen->grid (gs->screen @st)))