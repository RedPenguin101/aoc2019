(ns aoc2019.day23
  (:require [aoc2019.intcode :refer [parse-program run-program2 resume]]))

(def program (parse-program (slurp "resources/day23input")))

(defn run-packet [computers [addr x y]]
  (update computers addr #(-> % (resume x) (resume y))))

(def debug (atom nil))

(defn run [computers queue]
  (cond
    (= (ffirst queue) 255) (first queue)
    (empty? queue)
    (let [new-c (mapv #(resume % -1) computers)]
      (recur new-c (vec (partition 3 (mapcat :outputs new-c)))))
    :else (let [new-c (run-packet computers (first queue))]
            (recur new-c
                   (concat (rest queue) (partition 3 (get-in new-c [(ffirst queue) :outputs])))))))
(comment
  (time (run (mapv #(run-program2 program [%]) (range 50))
             []))
  ;; "Elapsed time: 214.224593 msecs"
  ;; (255 79967 15662)
  (second @debug))




