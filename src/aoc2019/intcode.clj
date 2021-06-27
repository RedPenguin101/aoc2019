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
   "

  "Day2:1202 Program Alarm
 An Intcode program is a list of integers separated by commas
 opcode - either 1, 2, or 99
 Opcode 1 adds together numbers read from two positions and stores the result in a third position
 Opcode 2 works exactly like opcode 1, except it multiplies
 
 ")

(defn add  [[_op a b c] memory] (assoc memory c (+ (memory a) (memory b))))
(defn mult [[_op a b c] memory] (assoc memory c (* (memory a) (memory b))))

(defn instruction [{:keys [pointer memory]}] (subvec memory pointer (+ 4 pointer)))

(defn process-next [{:keys [pointer memory] :as state}]
  (case (memory pointer)
    99 (assoc state :halted true)
    1  (-> state (assoc :memory (add  (instruction state) memory)) (update :pointer + 4))
    2  (-> state (assoc :memory (mult (instruction state) memory)) (update :pointer + 4))))

(defn process [state] (if (:halted state) state (recur (process-next state))))

(comment
  (process-next {:pointer 0 :memory [1 0 0 0 99]})
  (process-next {:pointer 4 :memory [2 0 0 0 99]})
  (process {:pointer 0 :memory [1 0 0 0 99]}))

(defn run-program [program] (process {:pointer 0 :memory program}))

;; tests

(deftest day2
  (are [halt-state program] (= halt-state (:memory (run-program program)))
    [2 0 0 0 99] [1,0,0,0,99]
    [2 3 0 6 99] [2,3,0,3,99]
    [2 4 4 5 99 9801] [2,4,4,5,99,0]
    [30 1 1 4 2 5 6 0 99] [1,1,1,4,99,5,6,0,99]
    [3500 9 10 70 2 3 11 0 99 30 40 50] [1,9,10,3,2,3,11,0,99,30,40,50]))

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