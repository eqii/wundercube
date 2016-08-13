(ns wundercube.core
  (:require [clojure.math.numeric-tower :as math]))


(defn not-nil? [x]
  (not (nil? x)))

(defn lazy-contains? [coll key]
  (boolean (some #(= % key) coll)))

(defn map-values [m f & args]
 (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))

(def pairs (for [x (range 64) y (range 64)] [x y]))

(def letters "AJFEAPUWOGMRMNXKDNSIFODSJEGIWKPREQMFRKIDDMIREOSDRTSLDKPISPOIJQDT")
             
(defn adjacent? [x y]
  (< (math/abs (- x y)) 2))

(defn is-pair-adjacent? [a b]
  (and (not (= a b))
       (adjacent? (mod a 4) (mod b 4))
       (adjacent? (quot (mod a 16) 4) (quot (mod b 16) 4))
       (adjacent? (quot a 16) (quot b 16))))
      
(defn get-adjacency-list [x]
  (map second (second x)))

;; For each letter index, return a list of adjacent indexes.
(defn get-adjacent-table []
  (let [adjacent-pairs (filter #(apply is-pair-adjacent? %) pairs)
        grouped (group-by first adjacent-pairs)
        sorted (sort-by first grouped)
        adjacent-table (map get-adjacency-list sorted)]
    adjacent-table))

(def adjacent-table (get-adjacent-table))

;; Return a map of letters with their corresponding indexes on the cube.
(defn get-letter-positions []
  (map-values
   (group-by second (map-indexed vector letters))
   (fn [x] (map first x))))

(def letter-positions (get-letter-positions))

;; Search words in the cube using a recursive backtracking algorithm.
(defn backtracking-search [word index visited previous]
  (if (= (count word) index)
    true
    (let [new-visited (if (not-nil? previous) (conj visited previous) #{})
          found (for [square (get letter-positions (nth word index))]
                  (when
                    (and
                      (or (nil? previous) 
                          (lazy-contains? (nth adjacent-table previous) square))
                      (not (contains? visited square))
                      (true? (backtracking-search word (inc index) new-visited square)))
                    true))]
      (some true? found))))

(defn search-word-from-cube [word]
  (backtracking-search word 0 #{} nil))
  
(defn -main []
  (let [words (line-seq (java.io.BufferedReader. (java.io.StringReader. (slurp "http://www.wunderdog.fi/s/words.txt"))))
        uppercased (map clojure.string/upper-case words)
        results (pmap search-word-from-cube uppercased)]
    (println (count (filter not-nil? results))))
  (shutdown-agents))

