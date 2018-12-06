
; Some optimizations:
; Before keeping start-idx O(n^2) too slow to use
; Base O(n) "Elapsed time: 317442.809807 msecs"
; RBB vectors "Elapsed time: 17984.571612 msecs"
; Type hints "Elapsed time: 17574.831288 msecs"

(require '[clojure.string :as string])
(require '[clojure.core.rrb-vector :as fv])

(defn ^boolean reactable?
  [^Character x
   ^Character y]
  (and (not (= x y))
       (= (string/lower-case x)
          (string/lower-case y))))

(defn react
  [s start-idx]
  (loop
      [front (fv/subvec s 0 (+ start-idx 1))
       back (fv/subvec s (+ start-idx 1))
       idx start-idx]
    (let [b (first back)]
      (cond
        (nil? b)
        [front (max 0 (- idx 1))]
        (reactable? (last front) b)
        [(fv/catvec (fv/subvec front 0 (- (count front) 1))
                    (fv/subvec back 1))
         (max 0 (- idx 1))]
        :else
        (recur (conj front b)
               (fv/subvec back 1)
               (+ idx 1))))))

(defn react-until-fixed
  [s tag]
  (loop
      [prior s
       start-index 0
       i 1]
    (let
        [[current new-index] (react prior start-index)]
      (if (= (mod i 100) 0)
        (println (str tag (count current)))
        nil)
      (if (= (count prior) (count current))
        current
        (recur current new-index (+ i 1))))))

(defn remove-unit
  [unit s]
  (filter #(and (not (= % unit))
                (not (= % (first (string/upper-case unit)))))
          s))

(defn optimal-removal
  [s]
  (let [units (set (map #(first (string/lower-case %)) s))]
    (first
     (sort
      (pmap
       (fn [unit]
         (println unit)
         [(count (react-until-fixed (into [] (remove-unit unit s)) unit)) unit])
       (sort units))))))

(def test-main (into [] "dabAcCaCBAcCcaDA"))

(defn read-file
  [filename]
  (into [] (string/trim-newline (slurp filename))))

(defn main
  [filename]
  (count (react-until-fixed (read-file filename) "")))

(defn main2
  [filename]
  (optimal-removal (read-file filename)))
