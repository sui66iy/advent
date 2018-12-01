
(require '[clojure.java.io :as io])

(defn read-input
  "Read input filename and return a sequence of integers"
  [filename]
  (with-open [rdr (io/reader filename)]
    (doall (map #(Integer. %) (line-seq rdr)))))

(defn compute-frequency
  "Sum all numbers from input file"
  [filename]
  (reduce + (read-input filename)))

(defn find-repeats
  "Find first frequency that repeats"
  [filename]
  (let [input (into [] (read-input filename))
        length (count input)]
    (loop [idx 0
           current 0
           seen (set '(0))]
      (let [freq (+ (nth input (mod idx length)) current)]
        (if (contains? seen freq)
          freq
          (recur (+ idx 1)
                 freq
                 (conj seen freq)))))))

