(ns aoc2019.intcode
  (:require [clojure.test :refer [deftest is are]]
            [clojure.string :as str]))

(comment
  "Intcode program terminology
   
   Intcode programs are given as a list of integers; these values are used 
   as the initial state for the computer's memory.
   
   A position in memory is called an address (for example, the first value in memory is at address 0)
   Opcodes (like 1, 2, or 99) mark the beginning of an instruction
   The values used immediately after an opcode, if any, are called the instruction's parameters
   The address of the current instruction is called the instruction pointer
   After an instruction finishes, the instruction pointer increases by the number of values in the instruction
   ")

"Day 5: Thermal Environment Supervision Terminal
 
 two new instructions
 Opcode 3 takes a single integer as input and saves it to the position given by its only parameter.
 Opcode 4 outputs the value of its only parameter."

(defn add  [[_op a b c] memory] (assoc memory c (+ (memory a) (memory b))))
(defn mult [[_op a b c] memory] (assoc memory c (* (memory a) (memory b))))
(defn input [input [_op a] memory] (assoc memory a input))
(defn output [[_op a] memory] (memory a))

(defn instruction [{:keys [pointer memory]}]
  #_(println [pointer memory])
  (subvec memory pointer (+ ({99 0, 1 4, 2 4, 3 2, 4 2} (memory pointer)) pointer)))

(defn process-next [{:keys [pointer memory inputs] :as state}]
  (let [opcode (memory pointer) instr (instruction state)]
    (-> (case opcode
          99 (assoc state :halted true)
          1  (assoc state :memory (add  instr memory))
          2  (assoc state :memory (mult instr memory))
          3  (-> state (assoc :memory (input (first inputs) instr memory)) (update :inputs rest))
          4  (update state :outputs conj (output instr memory)))
        (update :pointer + (count instr)))))

(defn process [state] (if (:halted state) state (recur (process-next state))))

(comment
  (-> {:pointer 0 :memory [3,0,4,0,99] :inputs '(5)}
      process-next
      process-next)
  (process {:pointer 0 :memory [3,0,4,0,99] :inputs '(5)})
  1)

(defn run-program
  ([program] (process {:pointer 0 :memory program}))
  ([program inputs] (process {:pointer 0 :memory program :inputs inputs})))

;; tests

(deftest day2
  (are [halt-state program] (= halt-state (:memory (run-program program)))
    [2 0 0 0 99] [1,0,0,0,99]
    [2 3 0 6 99] [2,3,0,3,99]
    [2 4 4 5 99 9801] [2,4,4,5,99,0]
    [30 1 1 4 2 5 6 0 99] [1,1,1,4,99,5,6,0,99]
    [3500 9 10 70 2 3 11 0 99 30 40 50] [1,9,10,3,2,3,11,0,99,30,40,50]))

(deftest day5
  (is (= '(123) (:outputs (run-program [3,0,4,0,99] [123])))))

;; program and parsing code

(defn parse-program [string] (mapv #(Long/parseLong %) (str/split (str/trim-newline string) #",")))

(comment
  "Part1: What value is left at position 0"
  (def program-day2 (parse-program (slurp "resources/day2input")))

  (-> program-day2
      (assoc 1 12)
      (assoc 2 2)
      (run-program)
      :memory
      first)
  ;; => 3562672

  "Part2: determine what pair of inputs produces the output 19690720."

  (remove nil? (for [a (range 0 100) b (range 0 100)]
                 (when (= 19690720 (-> program-day2 (assoc 1 a) (assoc 2 b)
                                       (run-program)
                                       :memory
                                       first))
                   (+ (* 100 a) b))))
  ;; => (8250)
  )