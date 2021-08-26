(ns aoc2019.day19
  (:require
   [aoc2019.coordinate :as co]
   [aoc2019.intcode :refer [parse-program boot2 run-program2 resume]]))

(def program (parse-program (slurp "resources/day19input")))

(comment
  (def result (into {} (for [x (range 50) y (range 50)]
                         [[x y] (if (= 1 (first (:outputs (run-program2 program [x y]))))
                                  "#" ".")])))

  (count (filter #{"#"} (vals (into {} (for [x (range 50) y (range 50)]
                                         [[x y] (if (= 1 (first (:outputs (run-program2 program [x y]))))
                                                  "#" ".")])))))

  (doall (map println (co/de-coordinate result))))

(comment
  "Part 2
 The beam gets wider as it travels away from the emitter;
 need to be a minimum distance away to fit a square of that size into the beam
 Find the 100x100 square closest to the emitter that fits entirely within the tractor beam;
 find the point closest to the emitter. 
 X*10000 + Y"

  "Obviously too big to brute force.
 need to figure out the equation for each 'edge' of the beam and maybe have
 some function 'is-on-edge? :: eq coord -> bool'
 I think it's right to say that the top right and bottom left points of the square
 will be on the respective edges, so you should be able to 'follow' the beam down
 and test until you get it."

  ()

  (doall
   (map println
        (co/de-coordinate (into {} (for [row (sort (group-by (comp second first) (filter #(= "#" (val %)) result)))]
                                     [[(first (ffirst (sort-by ffirst (second row)))) (first row)] "#"]))
                          ".")))

  (doall
   (map println
        (co/de-coordinate (into {} (for [row (sort (group-by (comp second first) (filter #(= "#" (val %)) result)))]
                                     [[(first (ffirst (reverse (sort-by ffirst (second row))))) (first row)] "*"]))
                          ".")))

  (remove nil? (doall (map println (co/de-coordinate (merge
                                                      (into {} (for [row (sort (group-by (comp second first) (filter #(= "#" (val %)) result)))]
                                                                 [[(first (ffirst (reverse (sort-by ffirst (second row))))) (first row)] "*"]))
                                                      (into {} (for [row (sort (group-by (comp second first) (filter #(= "#" (val %)) result)))]
                                                                 [[(first (ffirst (sort-by ffirst (second row)))) (first row)] "#"])))
                                                     "."))))

  (last (for [row (sort (group-by (comp second first) (filter #(= "#" (val %)) result)))]
          [[(first (ffirst (sort-by ffirst (second row)))) (first row)] "#"]))
  ;; => [[35 49] "#"] => x=35, y=49

  "y=49/35 * x"
  "x=35/49 * y"

  (def lower-edge (for [row (sort (group-by (comp second first) (filter #(= "#" (val %)) result)))]
                    [(first (ffirst (sort-by ffirst (second row))))
                     (first row)]))

  (def upper-edge (for [row (sort (group-by (comp second first) (filter #(= "#" (val %)) result)))]
                    [(first (ffirst (reverse (sort-by ffirst (second row)))))
                     (first row)]))
  lower-edge
  upper-edge

  (for [coord lower-edge]
    (let [calc-x #(long (Math/ceil (* % (/ 350 490))))
          x (first coord)
          y (second coord)
          calced-x (calc-x y)]
      [x y calced-x (cond (> calced-x x) :calced-too-high
                          (< calced-x x) :calced-too-low
                          :else :OK)]))


  (defn test-calc [reference a b]
    (let [calc-x #(long (Math/ceil (* % (/ a b))))]
      (frequencies (map (fn [[x y]]
                          (cond (> (calc-x y) x) :too-high
                                (< (calc-x y) x) :too-low
                                :else :ok))
                        reference))))

  (test-calc lower-edge 35 49)

  (defn direction [results]
    (cond (:too-high results) :too-high
          (:too-low results) :too-low
          :else :ok))

  (defn refine-calc [reference-coords a b prev it]
    (let [results (test-calc reference-coords a b)
          dir (direction results)
          flipped? (not= dir prev)]
      (cond
        (> it 100) :break
        (= (count reference-coords) (:ok results)) [a b]
        (and (:too-high results) (:too-low results)) [:shit! a b]

        (and flipped? (= :too-high prev)) (recur reference-coords (inc (* 2 a)) (* 2 b) dir (inc it))
        (and flipped? (= :too-low prev))  (recur reference-coords (dec (* 2 a)) (* 2 b) dir (inc it))

        (:too-high results) (recur reference-coords (dec a) b dir (inc it))
        (:too-low results)  (recur reference-coords (inc a) b dir (inc it)))))

  (refine-calc lower-edge 50 50 :too-high 0)
  ;; => [567 800]
  (test-calc lower-edge 567 800)
  ;; => {:ok 47}

  (refine-calc upper-edge 42 49 :too-high 0)
  ;; => :shit!
  (test-calc upper-edge 41 49)

  (into {} (map #(vector [(long (Math/ceil (* % (/ 41 49)))) %] "#") (range 50)))

  (co/print-grid (co/de-coordinate (merge (into {} (map #(vector [(long (Math/ceil (* % (/ 41 49)))) %] "#") (range 50)))
                                          (into {} (for [row (sort (group-by (comp second first) (filter #(= "#" (val %)) result)))]
                                                     [[(first (ffirst (reverse (sort-by ffirst (second row))))) (first row)] "*"])))
                                   ".")))

