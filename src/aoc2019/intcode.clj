(ns aoc2019.intcode
  (:require [clojure.test :refer [deftest is are testing]]
            [clojure.string :as str]
            [clojure.core.async :as a]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSTRUCTION FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  " Instruction functions are passed Instructions. These are in the form"

  '[[opcode [mode-a mode-b mode-c]] param-a param-b param-c]

  "Instructions functions are also passed the memory of the computer, and the input value (for input) and current pointer value (for jumps)"

  "The return either the new memory state of the computer, the output (for output), 
   or the new pointer position (for jumps)")

(defn- add [[[_ [pa pb]] a b c] memory]
  (let [va (if (= :imm pa) a (memory a))
        vb (if (= :imm pb) b (memory b))]
    (assoc memory c (+ va vb))))

(defn- mult [[[_ [pa pb]] a b c] memory]
  (let [va (if (= :imm pa) a (memory a))
        vb (if (= :imm pb) b (memory b))]
    (assoc memory c (* va vb))))

(defn- less-than [[[_op [pa pb]] a b c] memory]
  (let [va (if (= :imm pa) a (memory a))
        vb (if (= :imm pb) b (memory b))]
    (if (< va vb) (assoc memory c 1) (assoc memory c 0))))

(defn- equal-to [[[_op [pa pb]] a b c] memory]
  (let [va (if (= :imm pa) a (memory a))
        vb (if (= :imm pb) b (memory b))]
    (if (= va vb) (assoc memory c 1) (assoc memory c 0))))

(defn- input [input [_ a] memory]
  (if (nil? input)
    (throw (ex-info "Input cannot be nil for input instruction" {:mem-dump memory}))
    (assoc memory a input)))

(defn- output [[[_ [pa]] a] memory] (if (= :imm pa) a (memory a)))

(defn- jump-if-true [pointer [[_op [pa pb]] a b] memory]
  (let [va (if (= :imm pa) a (memory a))
        vb (if (= :imm pb) b (memory b))]
    (if (zero? va) (+ pointer 3) vb)))

(defn- jump-if-false [pointer [[_op [pa pb]] a b] memory]
  (let [va (if (= :imm pa) a (memory a))
        vb (if (= :imm pb) b (memory b))]
    (if (zero? va) vb (+ pointer 3))))

(defn- opcode+modes
  "Returns a tuple of opcode and modes, like [5 [:imm :pos :pos]]
   valus in the modes are given in the same order as the parameters they relate to"
  [long-opcode]
  (let [param-vals (quot long-opcode 100)]
    [(- long-opcode (* param-vals 100))
     (reverse (map #({0 :pos 1 :imm} (Character/digit % 10)) (format "%03d" param-vals)))]))

(defn- instruction
  "Given the pointer and memory state of an intcode computer, will return the current instruction
   The format is [opcode+modes param-a param-b param-c]
   the number of params are correct for the opcode"
  [{:keys [pointer memory]}]
  (let [[opcode modes] (opcode+modes (memory pointer))]
    (assoc (subvec memory pointer (+ ({99 0, 1 4, 2 4, 3 2, 4 2, 5 3, 6 3, 7 4, 8 4}
                                      opcode) pointer))
           0
           [opcode modes])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SYNCHRONOUS EXECUTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- process-next [{:keys [pointer memory inputs] :as state}]
  (let [[opcode] (opcode+modes (memory pointer))
        instr (instruction state)]
    (-> (case opcode
          99 (-> state (assoc :halted true))
          1  (-> state (assoc :memory (add  instr memory)) (update :pointer + (count instr)))
          2  (-> state (assoc :memory (mult instr memory)) (update :pointer + (count instr)))

          3  (-> state (assoc :memory (input (first inputs) instr memory))
                 (update :inputs rest)
                 (update :pointer + (count instr)))

          4  (-> state (update :outputs conj (output instr memory)) (update :pointer + (count instr)))

          5  (assoc state :pointer (jump-if-true pointer instr memory))
          6  (assoc state :pointer (jump-if-false pointer instr memory))

          7  (-> state (assoc :memory (less-than instr memory)) (update :pointer + (count instr)))
          8  (-> state (assoc :memory (equal-to instr memory)) (update :pointer + (count instr)))))))

(defn- process [state] (if (:halted state) state (recur (process-next state))))

(defn run-program
  ([program] (process {:pointer 0 :memory program}))
  ([program inputs] (process {:pointer 0 :memory program :inputs inputs})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ASYNCHRONOUS EXECUTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn boot
  "Boot up an intcode computer. Provide the program, a channel to receive input, and an atom where the state of 
   the computer can be viewed. Returns a channel where the computer will output. The computer will pause execution
   until its output is taken."
  [program in state-atom]
  (let [out (a/chan)]
    (a/go-loop [{:keys [pointer memory] :as state} {:pointer 0 :memory program}]
      (reset! state-atom state)
      (let [[opcode] (opcode+modes (memory pointer))
            instr (instruction state)]
        (-> (case opcode
              99 (do (a/close! in) (a/close! out) (reset! state-atom (-> state (assoc :halted true))))
              1  (recur (-> state (assoc :memory (add  instr memory)) (update :pointer + (count instr))))
              2  (recur (-> state (assoc :memory (mult instr memory)) (update :pointer + (count instr))))

              3  (recur (-> state
                            (assoc :memory (input (a/<! in) instr memory))
                            (update :pointer + (count instr))))

              4  (let [output (output instr memory)]
                   (a/>! out output)
                   (recur (-> state (update :outputs conj output) (update :pointer + (count instr)))))

              5  (recur (assoc state :pointer (jump-if-true pointer instr memory)))
              6  (recur (assoc state :pointer (jump-if-false pointer instr memory)))

              7  (recur (-> state (assoc :memory (less-than instr memory)) (update :pointer + (count instr))))
              8  (recur (-> state (assoc :memory (equal-to instr memory)) (update :pointer + (count instr))))))))
    out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest day2
  (are [halt-state program] (= halt-state (:memory (run-program program)))
    [2 0 0 0 99] [1,0,0,0,99]
    [2 3 0 6 99] [2,3,0,3,99]
    [2 4 4 5 99 9801] [2,4,4,5,99,0]
    [30 1 1 4 2 5 6 0 99] [1,1,1,4,99,5,6,0,99]
    [3500 9 10 70 2 3 11 0 99 30 40 50] [1,9,10,3,2,3,11,0,99,30,40,50]))

(deftest day5
  (is (= [1101 100 -1 4 99] (:memory (run-program [1101,100,-1,4,0]))))
  (is (= '(123) (:outputs (run-program [3,0,4,0,99] [123]))))
  (is (thrown? Exception (run-program [3,0,4,0,99] [])))

  (testing "opcodes"
    (is (= [2 '(:pos :pos :pos)] (opcode+modes 2)))
    (is (= [2 '(:imm :pos :pos)] (opcode+modes 102)))
    (is (= [2 '(:pos :imm :pos)] (opcode+modes 1002)))
    (is (= [2 '(:pos :pos :imm)] (opcode+modes 10002)))
    (is (= [2 '(:imm :imm :pos)] (opcode+modes 1102)))
    (is (= [2 '(:imm :pos :imm)] (opcode+modes 10102)))
    (is (= [2 '(:imm :imm :imm)] (opcode+modes 11102))))

  (testing "Immediate and position modes"
    (is (= [1002 4 3 4 99] (:memory (run-program [1002,4,3,4,33]))))

    (is (= 2  (first (:memory (run-program [1 5 6 0 99 1 1]))))) ;; pos pos, 1+1
    (is (= 6  (first (:memory (run-program [101 5 6 0 99 1 1])))))  ;; imm pos, 5+1
    (is (= 7  (first (:memory (run-program [1001 5 6 0 99 1 1])))))  ;; pos imm, 1+6
    (is (= 11 (first (:memory (run-program [1101 5 6 0 99 1 1])))))  ;; imm imm, 5+6

    (is (= 4  (first (:memory (run-program [2 5 6 0 99 2 2]))))) ;; pos pos, 2*2
    (is (= 10 (first (:memory (run-program [102 5 6 0 99 2 2])))))  ;; imm pos, 5*2
    (is (= 12 (first (:memory (run-program [1002 5 6 0 99 2 2])))))  ;; pos imm, 2*6
    (is (= 30 (first (:memory (run-program [1102 5 6 0 99 2 2])))))  ;; imm imm, 5*6


    (is (= 999 (first (:outputs (run-program [4 3 99 999])))))
    (is (= 3 (first (:outputs (run-program [104 3 99 999]))))))

  (testing "Less than and equal to"
    (are [program input output] (= output (first (:outputs (run-program program [input]))))
      [3,9,8,9,10,9,4,9,99,-1,8] 7 0
      [3,9,8,9,10,9,4,9,99,-1,8] 8 1
      [3,9,8,9,10,9,4,9,99,-1,8] 9 0

      [3,9,7,9,10,9,4,9,99,-1,8] 7 1
      [3,9,7,9,10,9,4,9,99,-1,8] 8 0
      [3,9,7,9,10,9,4,9,99,-1,8] 9 0

      [3,3,1108,-1,8,3,4,3,99] 7 0
      [3,3,1108,-1,8,3,4,3,99] 8 1
      [3,3,1108,-1,8,3,4,3,99] 9 0

      [3,3,1107,-1,8,3,4,3,99] 7 1
      [3,3,1107,-1,8,3,4,3,99] 8 0
      [3,3,1107,-1,8,3,4,3,99] 9 0))

  (testing "jumping"
    (are [program input output] (= output (first (:outputs (run-program program [input]))))
      [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] -7 1
      [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] 0 0
      [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] 7 1

      [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] -7 1
      [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] 0 0
      [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] 7 1

      [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31
       1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104
       999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
      7 999

      [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31
       1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104
       999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
      8 1000

      [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31
       1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104
       999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
      9 1001)))

(defn- take-until-closed
  "Given a channel, takes messages from that channel until it is closed
  then returns a sequence of all messages it receives"
  ([channel] (take-until-closed [] channel))
  ([result-seq channel]
   (let [message (a/<!! channel)]
     (if message
       (recur (conj result-seq message) channel)
       result-seq))))

(deftest async-boot-tests
  (let [st (atom {})]
    (take-until-closed (boot [1101 5 5 0 99] (a/chan) st))
    (is (:halted @st)))

  (is (= 4 (first (take-until-closed (boot [4 0 99] (a/chan) (atom {}))))))

  (let [st (atom {})
        in (a/chan)
        out (boot [3 0 99] in st)]
    (a/>!! in 5)
    (take-until-closed out)
    (is (:halted @st)))

  (let [st (atom {})
        in (a/chan)
        out (boot [3,0,4,0,99] in st)]
    (a/>!! in 123)
    (is (= 123 (first (take-until-closed out))))
    (is (:halted @st))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing and program code
;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

  "Day 5
   Part1: After providing 1 to the only input instruction and passing all the tests, 
   what diagnostic code does the program produce?"

  (def program-day5 (parse-program (slurp "resources/day5input")))

  (:outputs (run-program program-day5 [1]))
  ;; => (15426686 0 0 0 0 0 0 0 0 0)

  (:outputs (run-program program-day5 [5]))
  ;; => (11430197)
  )

(comment
  "Async version of day 5"

  (def program-day5 (parse-program (slurp "resources/day5input")))
  (def st (atom {}))

  (let [in (a/chan)
        out (boot program-day5 in st)]
    (a/>!! in 1)
    (take-until-closed out))
  ;; => [0 0 0 0 0 0 0 0 0 15426686]

  (let [in (a/chan)
        out (boot program-day5 in st)]
    (a/>!! in 5)
    (take-until-closed out))
  ;; => [11430197]
  )