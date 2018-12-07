
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

(defn manhattan-distance
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1))
     (Math/abs (- y2 y1))))

(defn unique-minimize
  [f xs]
  (let
      [scored (sort (map vector (map f xs) xs))]
    (if (= (first (first scored))
           (first (second scored)))
      nil ; if two or more are tied in score, return nil
      (second (first scored)))))

(defn min-distance
  "Find the unique point in xyz closest to xy or nil if no unique point"
  [xy xys]
  (unique-minimize (partial manhattan-distance xy) xys))

(defn all-coords-box
  [minx miny maxx maxy]
  (for [x (range minx (+ maxx 1))
        y (range miny (+ maxy 1))]
    [x y]))

(defn all-coords
  [xys]
  (let [xs (map first xys)
        ys (map second xys)]
    (all-coords-box (apply min xs) (apply min ys)
                    (apply max xs) (apply max ys))))

(defn closest-coords
  [xys]
  (let
      [all-xys (all-coords xys)]
    (map (fn [xy] (min-distance xy xys)) all-xys)))

(defn largest-area
  "Calculate largest non-infinite area"
  [xys]
  (let [all-xys (all-coords xys)
        all-xs (map first all-xys)
        all-ys (map second all-xys)
        minx (apply min all-xs)
        maxx (apply max all-xs)
        miny (apply min all-ys)
        maxy (apply max all-ys)
        is-infinite? (fn [[x y]]
                       (or (= x minx) (= x maxx) (= y miny) (= y maxy)))]
    (second (last
             (sort-by val
                      (filter #(and (not (nil? (key %)))
                                    (not (is-infinite? (key %))))
                              (frequencies (closest-coords xys))))))))

(defn total-distance
  [xy xys]
  (reduce + (map (partial manhattan-distance xy) xys)))

;; we could make this more efficient by adding an early stopping condition
;; to total-distance based on the dist provided to count-coords-near.  Since
;; we know that we only care about distances less than dist, once we have
;; convinced ourselves total-distance is greater than that we can stop!

(defn count-coords-near
  [xys dist]
  (let [coords (all-coords xys)]
    (count (filter #(< % dist) (pmap #(total-distance % xys) coords)))))

(def test-main [[1, 1]
                [1, 6]
                [8, 3]
                [3, 4]
                [5, 5]
                [8, 9]])

(defn parse-record
  [s]
  (into [] (map #(Integer. %) (str/split s #", "))))

(defn read-file
  [filename]
  (with-open [rdr (io/reader filename)]
    (into [] (map parse-record (line-seq rdr)))))

(defn main
  [filename]
  (largest-area (read-file "input.txt")))

(defn main2
  [filename]
  (count-coords-near (read-file "input.txt") 10000))
    
