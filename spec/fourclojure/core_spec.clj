(ns fourclojure.core-spec
  (:require [speclj.core :refer :all]
            [fourclojure.core :refer :all]))

;; Word chains (#82)

(describe "Word chains"
  (describe "intermediate functions"
    (it "'snuggle' determines if two words are part of a word chain"
      (should= (snuggle "cat" "car") true)
      (should= (snuggle "nag" "snag") true)
      (should= (snuggle "nag" "snags") false)
      (should= (snuggle "nag" "nag") false)
      (should= (snuggle "bat" "bait") true)
      (should= (snuggle "rabbit" "rabbi") true)))
          
  (describe "solution"
    (it "should identify word chains"
      (should= (word-chain? #{"hat"  "coat"  "dog"  "cat"  "oat"  "cot"  "hot"  "hog"}) true)
      (should= (word-chain? #{"cot"  "hot"  "bat"  "fat"}) false)
      (should= (word-chain? #{"to"  "top"  "stop"  "tops"  "toss"}) false)
      (should= (word-chain? #{"spout"  "do"  "pot"  "pout"  "spot"  "dot"}) true)
      (should= (word-chain? #{"share"  "hares"  "shares"  "hare"  "are"}) true)
      (should= (word-chain? #{"share"  "hares"  "hare"  "are"}) false))))

(describe "palindromic numbers"
  (it "should return a lazy sequence of palindromic numbers"
    (should-be-a clojure.lang.LazySeq (pal-seq 10000))
    (should= (take 26 (pal-seq 0)) [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101 111 121 131 141 151 161])
    (should= (take 16 (pal-seq 162)) [171 181 191 202 212 222 232 242 252 262 272 282 292 303 313 323])
    (should= (take 6 (pal-seq 1234550000)) [1234554321 1234664321 1234774321 1234884321 1234994321 1235005321])
    (should= (first (pal-seq (* 111111111 111111111))) (* 111111111 111111111))
    (should= (set (take 199 (pal-seq 0))) (set (map #(first (pal-seq %)) (range 0 10000))))
    (should= (apply <  (take 6666 (pal-seq 9999999))) true)))
