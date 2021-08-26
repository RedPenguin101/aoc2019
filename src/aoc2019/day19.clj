(ns aoc2019.day19
  (:require
   [aoc2019.coordinate :as co]
   [aoc2019.intcode :refer [parse-program run-program2]]))

(def program (parse-program (slurp "resources/day19input")))

(comment
  (def result (into {} (for [x (range 50) y (range 50)]
                         [[x y] (if (= 1 (first (:outputs (run-program2 program [x y]))))
                                  "#" ".")])))

  (count (filter #{"#"} (vals (into {} (for [x (range 50) y (range 50)]
                                         [[x y] (if (= 1 (first (:outputs (run-program2 program [x y]))))
                                                  "#" ".")])))))

  (doall (map println (co/de-coordinate result))))

(defn pulled? [x y] (= 1 (first (:outputs (run-program2 program [x y])))))

(comment
  (defn upper-y*
    ([x] (upper-y* x x))
    ([x y] (if (pulled? x y) y
               (recur x (inc y)))))

  (upper-y* 10000)
  ;; "Elapsed time: 10284.355772 msecs"
  ;; => 11547

  "This takes much too long - a starting assumption of x=y is no good. But we can improve it
   pretty easily by saying y=ax, 11547=a10000, a=11547/10000. Using a as our staring point, and 
   rounding down, should give us better performance. Actually round it down a bit so we don't 
   accidentally overshoot: a=1154/1000
   
   This gives a time of ~4200ms for an x of 10e6. This should be fine, but if we still see 
   performance issues, we can refine our estimate progressively")

(defn upper-y
  ([x] (upper-y x (int (/ (* 1154 x) 1000))))
  ([x y] (if (pulled? x y) y (recur x (inc y)))))

(defn avg [x y] (int (/ (+ x y) 2)))

(defn over-under [x]
  (let [square100? (fn [x y] (pulled? (- x 99) (+ y 99))) ;; keeping local scope for this fn
        fits-100-square? (square100? x (upper-y x))]
    (cond (not fits-100-square?) :under-shoot
          (and fits-100-square? (square100? (dec x) (upper-y (dec x)))) :over-shoot
          :else :found)))

(defn find-100-square
  ([x] (find-100-square x 0 nil))
  ([x lb ub]
   (case (over-under x)
     :under-shoot (recur (if ub (avg x ub) (* 2 x)) x ub) ;; guarding for a nil upper bound
     :over-shoot  (recur        (avg x lb)          lb x)
     :found       [x (upper-y x)])))

(comment
  (time (find-100-square 100))
  "Elapsed time: 426.602073 msecs"
  ;; => [937 1082]

  (+ (* (- 937 99) 10000) 1082)
  ;; => 8381082
  )


