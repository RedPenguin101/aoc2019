(ns aoc2019.day14
  (:require [clojure.string :as str]
            [ubergraph.core :as ug]
            [clojure.test :refer [deftest is]]))

(def debug (atom []))
(defn map-kv [f m] (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn parse-recipe [string]
  (let [ingreds (for [ingred (partition 2 (re-seq #"\w+" string))]
                  [(Long/parseLong (first ingred)) (second ingred)])
        [produces produced] (last ingreds)
        inputs (butlast ingreds)]
    [produced {:produces produces :inputs (into {} (mapv (comp vec reverse) inputs))}]))

(def input-recipes (into {} (map parse-recipe (str/split-lines (slurp "resources/day14input")))))
(def example1-recipies (into {} (map parse-recipe (str/split-lines "10 ORE => 10 A\n1 ORE => 1 B\n7 A, 1 B => 1 C\n7 A, 1 C => 1 D\n7 A, 1 D => 1 E\n7 A, 1 E => 1 FUEL"))))
(def example2-recipes (into {} (map parse-recipe (str/split-lines "9 ORE => 2 A\n8 ORE => 3 B\n7 ORE => 5 C\n3 A, 4 B => 1 AB\n5 B, 7 C => 1 BC\n4 C, 1 A => 1 CA\n2 AB, 3 BC, 4 CA => 1 FUEL"))))
(def example3-recipes (into {} (map parse-recipe (str/split-lines "157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"))))
(def example4-recipes (into {} (map parse-recipe (str/split-lines "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG\n17 NVRVD, 3 JNWZP => 8 VPVL\n53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL\n22 VJHF, 37 MNCFX => 5 FWMGM\n139 ORE => 4 NVRVD\n144 ORE => 7 JNWZP\n5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC\n5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV\n145 ORE => 6 MNCFX\n1 NVRVD => 8 CXFTF\n1 VJHF, 6 MNCFX => 4 RFSQX\n176 ORE => 6 VJHF\n"))))
(def example5-recipes (into {} (map parse-recipe (str/split-lines "171 ORE => 8 CNZTR\n7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n114 ORE => 4 BHXH\n14 VRPVC => 6 BMBT\n6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n5 BMBT => 4 WPTQ\n189 ORE => 9 KTJDG\n1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n12 VRPVC, 27 CNZTR => 2 XDBXC\n15 KTJDG, 12 BHXH => 5 XCVML\n3 BHXH, 2 VRPVC => 7 MZWV\n121 ORE => 7 VRPVC\n7 XCVML => 6 RJRHP\n5 BHXH, 4 VRPVC => 5 LTCX"))))

(comment
  "Take a quick look at the graph"
  (defn add-edge [graph [input recipe]] (concat graph (for [o (keys (:inputs recipe))] [input o])))
  (ug/viz-graph (apply ug/digraph (reduce add-edge [] example1-recipies)))
  (ug/viz-graph (apply ug/digraph (reduce add-edge [] input-recipes))))

(comment
  "Our representation of the ingredient list"
  example1-recipies ;; =>
  {"A" {:produces 10, :inputs {"ORE" 10}}
   "B" {:produces 1, :inputs {"ORE" 1}}
   "C" {:produces 1, :inputs {"A" 7, "B" 1}}
   "D" {:produces 1, :inputs {"A" 7, "C" 1}}
   "E" {:produces 1, :inputs {"A" 7, "D" 1}}
   "FUEL" {:produces 1, :inputs {"A" 7, "E" 1}}})

(defn depth-of [recipes material]
  (if (= "ORE" material) 0
      (->> (keys (:inputs (recipes material)))
           (map #(inc (depth-of recipes %)))
           (apply max))))

(defn largest-depth [recipes materials]
  (->> materials
       (map #(vector (depth-of recipes %) %))
       sort reverse first second))

(comment
  (depth-of example1-recipies "ORE")
  (depth-of example1-recipies "C")
  (depth-of example1-recipies "FUEL")
  (depth-of input-recipes "ORE")
  (depth-of input-recipes "FUEL")
  ;; our input has a max depth of 15 

  (largest-depth example1-recipies ["ORE" "A"])
  (largest-depth example1-recipies ["ORE" "A" "B" "C" "D" "E"]))

(defn produce [recipes to-produce amount]
  (let [batch-size (:produces (recipes to-produce))
        batches (long (Math/ceil (/ amount batch-size)))]
    (map-kv #(* batches %) (:inputs (recipes to-produce)))))

(defn break-down-materials
  "Given recipes to produce materials and a hashmap of materials to produce with the desired amount,
   successively break down those materials into their constituent input materials, 
   until only base ORE is left."
  [recipes materials]
  (if (= ["ORE"] (keys materials)) materials
      (let [material-to-produce (largest-depth recipes (keys materials))]
        (recur recipes (merge-with (fnil + 0)
                                   (dissoc materials material-to-produce)
                                   (produce recipes material-to-produce (materials material-to-produce)))))))

(comment
  (break-down-materials example1-recipies {"FUEL" 1})
  ;; => {"ORE" 31}
  (break-down-materials input-recipes {"FUEL" 1})
  ;; => {"ORE" 485720}
  "Genuinely didn't expect that to work...")

(deftest t
  (is (= {"ORE" 485720} (break-down-materials input-recipes {"FUEL" 1})))
  (is (= {"ORE" 31} (break-down-materials example1-recipies {"FUEL" 1})))
  (is (= {"ORE" 165} (break-down-materials example2-recipes {"FUEL" 1})))
  (is (= {"ORE" 13312} (break-down-materials example3-recipes {"FUEL" 1})))
  (is (= {"ORE" 180697} (break-down-materials example4-recipes {"FUEL" 1})))
  (is (= {"ORE" 2210736} (break-down-materials example5-recipes {"FUEL" 1}))))

(comment "Part 2
          Given 1 trillion ORE, what is the maximum amount of FUEL you can produce?"

         "MARK 1 BRAIN: Just try stuff until you get it :P"

         (break-down-materials example3-recipes {"FUEL" 82892753})
         ;; => {"ORE" 999999999076}
         (break-down-materials example4-recipes {"FUEL" 5586022})
         ;; => {"ORE" 999999895124}
         (break-down-materials example5-recipes {"FUEL" 460664})
         ;; => {"ORE" 999998346916}
         (> ((break-down-materials example5-recipes {"FUEL" 460664}) "ORE") 1000000000000)
         ;; => false
         (> ((break-down-materials example5-recipes {"FUEL" 460665}) "ORE") 1000000000000)
         ;; => true

         (> ((break-down-materials input-recipes {"FUEL" 1000000}) "ORE") 1000000000000)
         (> ((break-down-materials input-recipes {"FUEL" 3000000}) "ORE") 1000000000000)
         (> ((break-down-materials input-recipes {"FUEL" 3800000}) "ORE") 1000000000000)
         (> ((break-down-materials input-recipes {"FUEL" 3840000}) "ORE") 1000000000000)
         (> ((break-down-materials input-recipes {"FUEL" 3848000}) "ORE") 1000000000000)
         (> ((break-down-materials input-recipes {"FUEL" 3848998}) "ORE") 1000000000000)

         (break-down-materials input-recipes {"FUEL" 3848999})
         ;; => {"ORE" 1000000061058}
         (time (break-down-materials input-recipes {"FUEL" 3848998}))
         ;; => {"ORE" 999999822644}

         "less than 50 ms too - did I luck out on pt. 1? Or do much more than was required? :S")
