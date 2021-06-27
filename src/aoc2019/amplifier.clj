(ns aoc2019.amplifier
  (:require [aoc2019.intcode :refer [boot-with-input]]
            [clojure.string :as str]
            [clojure.core.async :as a]
            [clojure.math.combinatorics :as combo]))

(defn parse-program [string] (mapv #(Long/parseLong %) (str/split (str/trim-newline string) #",")))

(defn- take-until-closed
  "Given a channel, takes messages from that channel until it is closed
  then returns a sequence of all messages it receives"
  ([channel] (take-until-closed [] channel))
  ([result-seq channel]
   (let [message (a/<!! channel)]
     (if message
       (recur (conj result-seq message) channel)
       result-seq))))
(do
  (def st1 (atom {}))
  (def st2 (atom {}))
  (def st3 (atom {}))
  (def st4 (atom {}))
  (def st5 (atom {})))

(defn run-amplifiers [program [a b c d e]]
  (let [in (a/chan)
        o1 (boot-with-input program in st1 a)
        o2 (boot-with-input program o1 st2 b)
        o3 (boot-with-input program o2 st3 c)
        o4 (boot-with-input program o3 st4 d)
        out (boot-with-input program o4 st5 e)]
    (a/>!! in 0)
    (last (take-until-closed out))))

(comment
  (= 43210 (run-amplifiers [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] [4 3 2 1 0]))
  (= 54321 (run-amplifiers [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0] [0,1,2,3,4]))
  (= 65210 (run-amplifiers [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0] [1,0,4,3,2])))

(comment
  (= 43210 (apply max (for [perm (combo/permutations [0 1 2 3 4])] (run-amplifiers [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] perm))))
  (= 54321 (apply max (for [perm (combo/permutations [0 1 2 3 4])] (run-amplifiers [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0] perm))))
  (= 65210 (apply max (for [perm (combo/permutations [0 1 2 3 4])] (run-amplifiers [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0] perm))))

  (def input (parse-program (slurp "resources/day7input")))
  (apply max (for [perm (combo/permutations [0 1 2 3 4])] (run-amplifiers input perm)))
  ;; => 118936
  )

(defn run-amplifiers-looped [program [a b c d e]]
  (let [final (a/chan)
        in (a/chan)
        o1 (boot-with-input program in st1 a)
        o2 (boot-with-input program o1 st2 b)
        o3 (boot-with-input program o2 st3 c)
        o4 (boot-with-input program o3 st4 d)
        out (boot-with-input program o4 st5 e final)]
    (a/>!! in 0)
    (a/pipe out in)
    (a/<!! final)))

(defn bwi-wrap
  ([in program st input final] (boot-with-input program in st input final))
  ([in program st input] (boot-with-input program in st input)))

(defn run-amplifiers-looped2 [program [a b c d e]]
  (let [final (a/chan)
        in (a/chan)]
    (a/pipe (-> in (bwi-wrap program st1 a) (bwi-wrap program st2 b) (bwi-wrap program st3 c) (bwi-wrap program st4 d) (bwi-wrap program st5 e final))
            in)
    (a/>!! in 0)
    (a/<!! final)))

(comment
  (run-amplifiers-looped2 [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5] [9,8,7,6,5])
  ;; => 139629729

  (apply max (for [perm (combo/permutations [5 6 7 8 9])] (run-amplifiers-looped [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5] perm)))
  ;; => 139629729

  (def input (parse-program (slurp "resources/day7input")))
  (apply max (for [perm (combo/permutations [5 6 7 8 9])] (run-amplifiers-looped input perm)))
  ;; => 57660948
  )