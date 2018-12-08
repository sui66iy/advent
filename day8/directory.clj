
(require '[clojure.string :as string])

(defn read-node
  [data]
  (let [n-children (first data)
        n-metadata (second data)]
    (loop [idx 0
           child-sum 0
           rest-data (subvec data 2)]
      (if (< idx n-children)

        (let [[next-child-sum next-rest-data] (read-node rest-data)]
          (recur (+ idx 1)
                 (+ child-sum next-child-sum)
                 next-rest-data))
        [(reduce + child-sum (subvec rest-data 0 n-metadata))
         (subvec rest-data n-metadata)]))))

(defn get-child-value
  [child-values idx]
  (try
    (nth child-values (- idx 1))
    (catch Exception e 0)))

(defn value-node
  [data]
  (let [n-children (first data)
        n-metadata (second data)]
    (if (= n-children 0)
      ;; this node has no children, so sum the metadata values and return
      [(reduce + (subvec data 2 (+ n-metadata 2)))
       (subvec data (+ n-metadata 2))]
      
      (loop [idx 0
             child-values []
             rest-data (subvec data 2)]
        
        (if (< idx n-children)
          (let [[next-child-value next-rest-data] (value-node rest-data)]
            (recur (+ idx 1)
                   (conj child-values next-child-value)
                   next-rest-data))

          [(reduce + (map #(get-child-value child-values %)
                          (subvec rest-data 0 n-metadata)))
           (subvec rest-data n-metadata)])))))
    
(def test-main [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])

(defn read-file
  [filename]
  (into []
        (map #(Integer. %)
             (string/split (string/trim-newline (slurp filename)) #" "))))

(defn main
  [filename]
  (first (read-node (read-file filename))))

(defn main2
  [filename]
  (first (value-node (read-file filename))))
