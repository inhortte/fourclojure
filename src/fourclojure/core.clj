(ns fourclojure.core)

;; 4clojure #82  (word chains) 

(defn snuggle [uno dos]
  (loop [[up & ur :as u] uno [dp & dr :as d] dos err 0]
    (let [uc (count u) dc (count d)]
      (cond (or (nil? up) (nil? dp))
                (let [diff (Math/abs (- uc dc))]
                  (and (< diff 2)
                       (or (and (zero? err) (= 1 diff))
                           (and (= err 1) (zero? diff)))))
            (= up dp) (recur ur dr err)
            (not= err 0) false
            (= uc dc) (recur ur dr (inc err))
            (< uc dc) (recur ur (rest dr) (inc err))
            (< uc dc) (recur ur  (rest dr) (inc err))
            :else  (recur (rest ur) dr (inc err))))))

(defn perms [xs]
  (cond (empty? xs) ()
        (= (count xs) 1) (list (seq xs))
        :else (for [x xs
                    y (perms (disj (set xs) x))]
                (do
                  (cons x y)))))

(defn word-chain? [ws]
  (letfn [(snug [uno dos]
            (loop [[up & ur :as u] uno [dp & dr :as d] dos err 0]
              (let [uc (count u) dc (count d)]
                (cond (or (nil? up) (nil? dp))
                          (let [diff (Math/abs (- uc dc))]
                            (and (< diff 2)
                                (or (and (zero? err) (= 1 diff))
                                    (and (= err 1) (zero? diff)))))
                      (= up dp) (recur ur dr err)
                      (not= err 0) false
                      (= uc dc) (recur ur dr (inc err))
                      (< uc dc) (recur ur (rest dr) (inc err))
                      (< uc dc) (recur ur  (rest dr) (inc err))
                      :else  (recur (rest ur) dr (inc err))))))
          (pms [xs]
            (cond (empty? xs) ()
                  (= (count xs) 1) (list (seq xs))
                  :else (for [x xs
                              y (pms (disj (set xs) x))]
                          (do
                            (cons x y)))))]
    (->> (pms ws)
         (map (partial partition 2 1))
         (map (fn [ws] (map (partial apply snug) ws)))
         (map (partial every? true?))
         (some true?)
         ((complement nil?)))))
