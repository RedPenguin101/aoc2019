(ns aoc2019.day25
  (:require [clojure.set :refer [map-invert]]
            [clojure.math.combinatorics :refer [subsets]]
            [aoc2019.intcode :refer [parse-program run-program2 resume output->screen int->ascii]]))

(def program (parse-program (slurp "resources/day25input")))

(defn translate-message [message]
  (map (map-invert int->ascii) (str message \newline)))

(comment

  (->> ["north" "north"] ;; etc.
       (mapcat translate-message)
       (run-program2 program)
       :outputs
       output->screen
       (take-last 12))

  ;; just fiddled around with the commands and drew this map 

  "XX         KT = CO = NV
   SC = HW    SB    |
        SG ======= OB
                   HD = WD = CQ
           PS ==== HB
           EG   AR HC
            |  
           SL ==== ST
           GW")

(def to-pressure-plate
  ["south"                       "take hologram"
   "north" "west"                "take mutex"
   "south" "south" "south"       "take polygon"
   "north" "east"                "take weather machine"
   "west" "north" "north" "east"
   "north" "north" "north"       "take semiconductor"
   "east"                        "take prime number"
   "west" "west"                 "take monolith"
   "east" "south" "west" "north" "take jam"
   "west" "inv"])

;; the state of the machine after picking up all items
;; (that can be picked up) and positioning to the south
;; of the pressure plate
(defonce pp
  (assoc (run-program2 program (mapcat translate-message to-pressure-plate))
         :outputs []))

(def inv ["mutex"
          "hologram"
          "polygon"
          "jam"
          "semiconductor"
          "prime number"
          "monolith"
          "weather machine"])

;; there are 8 items, so 2^8 = 256 subsets
;; small enough to brute force easily.

;; this will create a list of commands that
;; will drop the passed list of objects and try
;; to move north
(defn create-commands [objects-to-drop]
  (conj (mapv #(str "drop " %) objects-to-drop)
        "north"))

;; pass in objects, create the drop commands,
;; get the output
(defn test-drops [objects]
  (->> (create-commands objects)
       (mapcat translate-message)
       (assoc pp :inputs)
       (resume)
       (:outputs)
       (output->screen)
       (take-last 20)))

(def results (map (juxt identity test-drops) (subsets inv)))

(comment
  "Just hacked through the results until I got a success 
   message which had the code in in."

  (let [x 160]
    (drop (- x 10) (take x results)))

  (take-last 3 (test-drops '("polygon" "prime number" "monolith" "weather machine")))

  "A loud, robotic voice says Analysis complete You may proceed. and you enter the cockpit."
  "Santa notices your small droid, looks puzzled for a moment, realizes what has happened, and radios your ship directly."
  "Oh, hello You should be able to get in by typing 35717128 on the keypad at the main airlock.")
