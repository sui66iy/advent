
(require '[clojure.java.io :as io])

(defn count-letters
  [s]
  (let [count-letter (fn [counts ch]
                       (if (contains? counts ch)
                         (assoc counts ch (+ (counts ch) 1))
                         (assoc counts ch 1)))]
                       
    (reduce count-letter {} s)))

(defn hasN
  "Return 1 if the string contains exactly N copies of some
  character"
  [s n]
  (if (contains? (into #{} (vals (count-letters s))) n)
    1
    0))

(defn has3
  [s]
  (hasN s 3))

(defn has2
  [s]
  (hasN s 2))

(defn checksum
  [ids]
  (let
      [count3 (reduce + (map has3 ids))
       count2 (reduce + (map has2 ids))]
    (* count3 count2)))
      
(defn main
  "Read input filename and return checksum"
  [filename]
  (with-open [rdr (io/reader filename)]
    (checksum (line-seq rdr))))

