(ns aoc2019.day19
  (:require
   [aoc2019.coordinate :as co]
   [aoc2019.intcode :refer [parse-program run-program2]]))

(def program (parse-program (slurp "resources/day19input")))

;; Part 1

(defn pulled? [x y] (= 1 (first (:outputs (run-program2 program [x y])))))

(comment
  "Finding the 'start' of the beam"

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
   performance issues, we can refine our estimate progressively"

  "We could improve further with a O(logn) squeezing thing instead of the O(n) inc y, 
  but probably not worth it.")

(defn upper-y
  ([x] (upper-y x (int (/ (* 1154 x) 1000))))
  ([x y] (cond
           (> y (* 2 x)) nil
           (pulled? x y) y
           :else (recur x (inc y)))))

(defn bounds [x]
  (when-let [upper (upper-y x)]
    (loop [y upper]
      (cond
        (>= y 50) (- y upper)
        (pulled? x y) (recur (inc y)) ;; likewise this (inc y) is inefficient - but fine for now
        :else (- y upper)))))

(comment
  (time (apply + (keep bounds (range 0 50))))
  "Elapsed time: 2457.091798 msecs"
  ;; => 192

  "from counting the calls to pulled in this"
  ;; => 325

  "Probably good enough. The intcode computer is the bottleneck here - about 6.5ms per call"
  (time (dotimes [n 1000] (pulled? n n)))
  "Elapsed time: 6380.697862 msecs"
  (float (/ 6380 1000))
  ;; => 6.38ms per call to pulled

  "and 325 calls to pulled in the solution"
  (* 325 6.38)
  ;; => 2073.5
  "To improve this it would be necessary to reduce the number of calls to pulled, which could 
   easily be done by improving the guess of where the upper and lower bounds are. Theoretically
   you could get it down to 200 guesses - 2 for the upper bound, 2 for the lower bound, 50 xs"
  (* 200 6.38)
  ;; => 1276.0
  "but not worth it IMO")

;; Part 2

(defn avg [x y] (int (/ (+ x y) 2)))

(defn squeeze
  "Given a predicate which returns -1,0, or 1 corresponding to 'too-low', 'found', 'too-high' 
   respectively, and a starting x value, will find the value that satisfies the predicate in
   O(log n) time, assuming a single solution. Can optionally take a custom averaging function."
  ([pred x] (squeeze pred #(/ (+ %1 %2) 2) x 0 nil))
  ([pred avg x] (squeeze pred avg x 0 nil))
  ([pred avg x lb ub]
   (case (pred x)
     -1 (recur pred avg (if ub (avg x ub) (* 2 x)) x ub)
     1  (recur pred avg        (avg x lb)          lb x)
     0  x)))

(defn over-under [x]
  (let [square100? (fn [x y] (pulled? (- x 99) (+ y 99))) ;; keeping local scope for this fn
        fits-100-square? (square100? x (upper-y x))]
    (cond (not fits-100-square?) -1
          (and fits-100-square? (square100? (dec x) (upper-y (dec x)))) 1
          :else 0)))

(comment
  (time (squeeze over-under avg 100))
  "Elapsed time: 426.602073 msecs"
  ;; => [937 1082]

  (+ (* (- 937 99) 10000) 1082)
  ;; => 8381082
  )
