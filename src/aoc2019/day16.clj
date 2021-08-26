(ns aoc2019.day16
  (:require [clojure.test :refer [deftest is]]))

(comment
  "Day 16: Flawed Frequency Transmission
   
   Signal quality. Clean with Flawed Frequency Transmission (FFT) algorithm
   
   Phases: new list constructed on each pass
   Used as input for next pass
   element in new list = old element * cycling pattern, keep last digit
   
   Pattern:
   depends on which output element is being calculated.
   base pattern is 0, 1, 0, -1.
   repeat each value in the pattern a number of times equal to the position in the output list
   When applying the pattern, skip the very first value exactly once. 
   (In other words, offset the whole pattern left by one.)
   
   After 100 phases of FFT, what are the first eight digits in the final output list?")

(def base-pattern [0 1 0 -1])
(defn create-pattern [position pattern] (mapcat #(repeat (inc position) %) pattern))
(defn digitize [string] (map (comp #(Long/parseLong %) str) string))

(def input (digitize (slurp "resources/day16input")))

(create-pattern 1 [0 1 0 -1])

(defn fft [input pattern]
  (for [i (range (count input))]
    (mod (Math/abs (apply + (map * input (drop 1 (cycle (create-pattern i pattern)))))) 10)))

(defn fft-iter [input pattern i]
  (if (zero? i) input (recur (fft input pattern) pattern (dec i))))

(deftest t
  (is (= '([1 2 3 4 5 6 7 8] (4 8 2 2 6 1 5 8) (3 4 0 4 0 4 3 8) (0 3 4 1 5 5 1 8) (0 1 0 2 9 4 9 8))
         (take 5 (iterate #(fft % base-pattern) [1 2 3 4 5 6 7 8]))))
  (is (= [2 4 1 7 6 1 7 6] (take 8 (fft-iter (digitize "80871224585914546619083218645595") base-pattern 100))))
  (is (= [7 3 7 4 5 4 1 8] (take 8 (fft-iter (digitize "19617804207202209144916044189917") base-pattern 100))))
  (is (= [5 2 4 3 2 1 3 3] (take 8 (fft-iter (digitize "69317163492948606335995924319873") base-pattern 100)))))

(comment
  (time (fft-iter input base-pattern 100))
  ;; => (1 9 2 3 9 4 6 8 1 4 4 6 4 4 1 9 4 4 0 ...
  ;; (11 seconds)
  )

(comment
  "The real signal is your puzzle input repeated 10000 times.
   The first seven digits of your initial input signal also represent the message offset.
   the location of the eight-digit message in the final output list"

  (apply str (take 7 input))
  ;; => 5,979,067 
  (float (/ (* 11 10000) 60 60))
  ;; => 30 hours, brute force not going to work

  (* 10000 (count input))
  ;; => 6,500,000 terms in input

  (create-pattern 2 base-pattern)

  "From https://work.njae.me.uk/2019/12/20/advent-of-code-2019-day-16/
   When we're calculating the last digit in the new message, the pattern for that new digit will 
   be many zeros and one one: it will look like [0, 0 … 0, 1]. That means the last digit of the message 
   remains the same. 

   The second-from-last digit in the new message is the last digit in the new message plus the 
   second-from-last digit in the old message (mod 10).
   
   The third-from-last digit in the new message is the second-from-last digit in the new message plus 
   the third-from-last digit in the old message.

   In other words, a message that ends abcdef will become a message that ends ghijkl, like this:
   a+b+c+d+e+f = g = a+h
     b+c+d+e+f = h = b+i
       c+d+e+f = i = c+j
         d+e+f = j = d+k
           e+f = k = e+l
             f = l"

  (apply str (fft (digitize "80871224585914546619083218645595") base-pattern))
   ;; => "24706861300603878265668532[484945]"

  "abcdef 645595
    
    new f =               5
    new e = (9 + 5 = 14)  4 
    new d = (5 + 14 = 19) 9
    new c = (5 + 19 = 24) 4
    new b = (4 + 4 = 8)   8
    new a = (6 + 8 = 14)  4

    484945
    "

  (apply str (fft (digitize "19617804207202209144916044189917") base-pattern))
  ;; => "43831307100027207873909339[546787]"


  "abcdef 189917
    
    new f =  7            7
    new e = (1 + 7 = 8)   8      
    new d = (9 + 8 = 17)  7     
    new c = (9 + 7 = 16)  6      
    new b = (8 + 6 = 14)  4     
    new a = (1 + 4 = 5)   5      

    546787
    ")

;; note, all of this will require input to be reversed then un-reversed at the end.

(defn fast-fft [digits] (reductions #(mod (+ %1 %2) 10) digits))

(comment
  (apply str (reverse (fast-fft [5 9 5 5 4 6])))
  ;; => "484945"
  (apply str (reverse (fast-fft [7 1 9 9 8 1])))
  ;; => "546787"
  )

(defn real-fft [input]
  (let [to-drop (Long/parseLong (apply str (take 7 input)))
        real-input (drop to-drop (take (* 10000 (count input)) (cycle (digitize input))))]
    (apply str (reverse (take-last 8 (last (take 101 (iterate fast-fft (reverse real-input)))))))))

(deftest t2
  (is (= "84462026" (real-fft "03036732577212944063491565474664")))
  (is (= "78725270" (real-fft "02935109699940807407585447034323")))
  (is (= "53553731" (real-fft "03081770884921959731165446850517"))))

(comment
  (time (real-fft (slurp "resources/day16input")))
  ;; => "96966221"
  ;; about 4.5 seconds
  )