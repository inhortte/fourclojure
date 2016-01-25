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
    ()))

