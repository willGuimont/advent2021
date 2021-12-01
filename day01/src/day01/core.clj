(ns day01.core
  (:gen-class))

(defn load-data [path]
  (with-open [rdr (clojure.java.io/reader path)]
    (doall (map #(Integer/parseInt % 10) (line-seq rdr)))))

(defn conseq [n xs]
  (partition n (apply interleave (for [i (range n)] (drop i xs)))))

(def bool-to-int {false 0 true 1})

(defn part-one [xs]
  (reduce +
          (map #(bool-to-int (apply < %)) (conseq 2 xs))))

(defn part-two [xs]
  (let [windowed (conseq 3 xs)
        sum (map #(apply + %) windowed)]
    (part-one sum)))

(defn -main
  "Advent of Code - Day 01"
  [& args]
  (let [xs (seq (load-data "input.txt"))]
    (println (part-one xs))
    (println (part-two xs))))
