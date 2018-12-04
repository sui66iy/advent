
(require '[clojure.java.io :as io])

(defrecord Box [id x y w h])

(defn parse-box
  "Create a Box from a string of the form #ID @ x,y: wxh"
  [s]
  (let [parsed (re-matches #"#(.*) @ (\d+),(\d+): (\d+)x(\d+)" s)]
    (apply #(Box. %1 %2 %3 %4 %5)
           (map #(Integer. %) (nthrest parsed 1)))))

(defn xx
  "Get the (x+w) coordinate"
  [box]
  (+ (:x box) (:w box)))

(defn yy
  "Get the (y+h) coordinate"
  [box]
  (+ (:y box) (:h box)))

(defn all-coords-in-box
  "Generate all coordinates contained in the box"
  [box]
  (for [x (range (:x box) (xx box))
        y (range (:y box) (yy box))]
    [x y]))

(defn inc-nil
  [x]
  (if (nil? x)
    1
    (inc x)))

(defn coord-counts
  "Construct a map of coordinates to the number of boxes that
  contain that coordinate"
  [the-boxes]
  (loop [coord-count {}
         boxes the-boxes]
    (let
        [box (first boxes)]
      (if (nil? box)
        coord-count
        (recur
         (reduce (fn [m xy]
                   (update m xy inc-nil))
                 coord-count
                 (all-coords-in-box box))
         (rest boxes))))))

(defn count-overlap-inches
  "Count the number of coordinates occupied by at least two
  boxes"
  [boxes]
  (count (filter #(>= % 2) (vals (coord-counts boxes)))))

(defn non-overlapping?
  "Check to see if a box overlaps with any others"
  [box coord-count]
  (every? #(= (coord-count %) 1) (all-coords-in-box box)))

(defn find-nonoverlapping-boxes
  "Find any boxes that are non-overlapping"
  [boxes]
  (let [coord-count (coord-counts boxes)]
    (filter #(not (nil? %))
            (map (fn [box]
                   (if (non-overlapping? box coord-count)
                     (:id box)
                     nil)) boxes))))

;; The next implementation is really slow!
;; Runtime is the total number of coordinates in the
;; bounding box times the number of boxes.
;; The above is linear in the number of coordinates
;; actually occupied by boxes.

(defn in-box?
  "Test to see if the point x, y is inside the box"
  [box [x y]]
  (and (<= (:x box) x)
       (<= (:y box) y)
       (< x (xx box))
       (< y (yy box))))

(defn bounds
 "Return a box that contains all the boxes"
 [boxes]
 (let [x (apply min (map :x boxes))
       y (apply min (map :y boxes))
       w (- (apply max (map xx boxes)) x)
       h (- (apply max (map yy boxes)) y)]
   (Box. x y w h nil)))

(defn count-true
  "Count the number of times true appears in booleans"
  [booleans]
  (reduce + (map #(if % 1 0) booleans)))

(defn count-overlap-inches-slow
  [boxes]
  (let [bbox (bounds boxes)
        all-coords (all-coords-in-box bbox)]
    (count
     (filter #(>= % 2)
             (map (fn [coord]
                    (count-true (map #(in-box? % coord) boxes)))
                  all-coords)))))

;; I/O and testing

(defn get-boxes
  [filename]
  (with-open [rdr (io/reader filename)]
    (into [] (line-seq rdr))))

(def test-data
  ["#1 @ 1,3: 4x4"
   "#2 @ 3,1: 4x4"
   "#3 @ 5,5: 2x2"])

(defn test1
  []
  (= 4 (count-overlap-inches (map parse-box test-data))))

(defn test2
  []
  (= 3 (first (find-nonoverlapping-boxes (map parse-box test-data)))))

(defn main
  [filename]
  (count-overlap-inches (map parse-box (get-boxes filename))))

(defn main2
  [filename]
  (find-nonoverlapping-boxes (map parse-box (get-boxes filename))))
  
