(ns aoc2019.day23
  (:require [aoc2019.intcode :refer [parse-program run-program2 resume]]))

(def program (parse-program (slurp "resources/day23input")))

(defn run-packet [computers [addr x y]]
  (update computers addr #(-> % (resume x) (resume y))))


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
  )

(defn send-1s [cs]
  (let [ncs (mapv #(resume % -1) cs)]
    [ncs (vec (partition 3 (mapcat :outputs ncs)))]))

(defn proc-pck [cs [[addr x y] & q]]
  (let [new-cs (update cs addr #(-> % (resume x) (resume y)))]
    [new-cs
     (concat q (partition 3 (get-in new-cs [addr :outputs])))]))

(defn run2
  "packet addr 255?                    update NAT
   packet?                         process packet
   No cycle?                             Send -1s
   last-nat not repeat?                  Fire NAT
   Else                                 Terminate"
  ([cs] (run2 [cs []] false nil nil))
  ([[cs [pck & rst :as q]] cycle nat p-nat]
   (cond
     (= 255 (first pck))     (recur [cs rst] false (rest pck) p-nat)
     pck                     (recur (proc-pck cs q) false nat p-nat)
     (not cycle)             (recur (send-1s cs) true nat p-nat)
     (not= (last nat) p-nat) (recur (proc-pck cs [(into [0] nat)]) false nat (last nat))
     :else                   p-nat)))


(comment
  (time (run2 (mapv #(run-program2 program [%]) (range 50))))
  ;; "Elapsed time: 3594.155488 msecs"
  ;; 10854
  )