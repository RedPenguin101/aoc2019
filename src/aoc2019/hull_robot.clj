(ns aoc2019.hull-robot
  (:require [aoc2019.intcode :refer [boot]]
            [clojure.string :as str]
            [clojure.core.async :as a]))

(defn parse-program [string] (mapv #(Long/parseLong %) (str/split (str/trim-newline string) #",")))

(defn move-forward [[x y] direction-facing]
  (case direction-facing
    0 [x (inc y)]
    1 [(inc x) y]
    2 [x (dec y)]
    3 [(dec x) y]))

(defn tick [robot-state color-to-paint direction-to-turn]
  (let [new-direction (mod (+ (:direction robot-state) (if (= 0 direction-to-turn) -1 1)) 4)]
    (-> robot-state
        (assoc-in [:painted-tiles (:location robot-state)] color-to-paint)
        (assoc :direction new-direction)
        (update :location move-forward new-direction))))

(tick {:painted-tiles {}
       :location [0 0]
       :direction 0}
      1 0)

(defn current-panel-color [robot-state]
  (get (:painted-tiles robot-state) (:location robot-state) 0))

(def st (atom {}))

(defn run-robot [robot ic-in ic-out]
  (a/>!! ic-in (current-panel-color robot))
  (let [color (a/<!! ic-out)
        dir   (a/<!! ic-out)
        new-robot (tick robot color dir)]
    (if (and color dir (current-panel-color new-robot))
      (recur new-robot ic-in ic-out)
      robot)))

(comment
  (def init-state {:painted-tiles {} :location [0 0] :direction 0})
  (def program (parse-program (slurp "resources/day11input")))

  (def output (let [in (a/chan 1)
                    out (boot program in st)]
                (run-robot init-state in out)))

  (count (:painted-tiles output))
  ;; => 1967
  @st

  (assoc-in init-state [:painted-tiles [0 0]] 1)
  (dissoc @st :memory)
  (get (:memory @st) 327)
  (get (:memory @st) 328)
  (def x @st)

  (def output (let [in (a/chan 1)
                    out (boot program in st)]
                (run-robot {:painted-tiles {[0 0] 1} :location [0 0] :direction 0} in out)))

  (def white-tiles (map first ((group-by val (:painted-tiles output)) 1)))


  1)
