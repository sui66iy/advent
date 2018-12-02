
(require '[clojure.java.io :as io])

(defn index-substring
  [s idx]
  (str (subs s 0 idx) (subs s (+ idx 1))))

(defn substrings
  "Generate all the index-substring examples for each possible
  index"
  [s]
  (map #(index-substring s %) (range (.length s))))

(defn common-element
  "Takes two sequences.  If they have a common element at a
  common position, it will return the first such example.
  Otherwise nil."
  [xs ys]
  (first
   (first
    (filter #(= (first %) (second %)) (map vector xs ys)))))

(defn correct-letters
  [ids]
  (let
      [all-substrings (map substrings ids)]
    (loop
        [current (first all-substrings)
         rest-substrings (rest all-substrings)]
      (let
          [common (set (map #(common-element current %) rest-substrings))]
        (do
          (if (= common #{nil})
            (recur (first rest-substrings)
                   (rest rest-substrings))
            (first (disj common nil))))))))

(def test-xs ["abcde"
              "fghij"
              "klmno"
              "pqrst"
              "fguij"
              "axcye"
              "wvxyz"])

(defn main
  "Read input filename and return common letters for box ids"
  [filename]
  (with-open [rdr (io/reader filename)]
    (correct-letters (line-seq rdr))))

