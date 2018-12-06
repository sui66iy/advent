
; Some optimizations:
; Before keeping start-idx O(n^2) too slow to use
; Base O(n) "Elapsed time: 317442.809807 msecs"
; RBB vectors "Elapsed time: 17984.571612 msecs"
; Type hints "Elapsed time: 17574.831288 msecs"

(require '[clojure.string :as string])
(require '[clojure.core.rrb-vector :as fv])

(defn ^boolean reactable?
  "Do these two characters 'react'?  E.g., Aa does, aA does,
  Ab does not, nor does AA."
  [^Character x
   ^Character y]
  (and (not (= x y))
       (= (string/lower-case x)
          (string/lower-case y))))

(defn react
  "Find a reactable pair of characters in the vector s and remove them,
  starting your search at start-idx.

  Return the new vector and an index at which to start your next search."
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
  "Keep removing reactable pairs of characters from the vector s until
  there no such pairs.
  Return the resulting vector (with all reactable pairs removed).
  tag is a string to include when printing status."  
  [s tag]
  (loop
      [prior s
       start-index 0
       i 1]
    (let
        [[current new-index] (react prior start-index)]
      (when (= (mod i 100) 0)
        (println tag (count current))) ; print out some progress periodically!
      (if (= (count prior) (count current))
        current
       (recur current new-index (+ i 1))))))

(defn remove-unit
  "Given a lower-case character, removes all instances of that character and its
  upper-case variant from the vector s, and returns the new vector."
  [unit s]
  (into []
        (filter #(and (not (= % unit))
                      (not (= % (first (string/upper-case unit)))))
                s)))

(defn optimal-removal
  "Find the shortest possible vector obtained after fully reacting every
  possible vector obtained by removing one type of character (unit).
  Return the length of that vector and the unit removed to obtain it."
  [s]
  (first
   (sort
    (pmap
     (fn [unit]
       [(count (react-until-fixed (remove-unit unit s) unit)) unit])
     (set (map #(first (string/lower-case %)) s))))))

(def test-main (into [] "dabAcCaCBAcCcaDA")) ; example test data

(defn read-file
  [filename]
  (into [] (string/trim-newline (slurp filename))))

(defn main
  [filename]
  (count (react-until-fixed (read-file filename) "")))

(defn main2
  [filename]
  (optimal-removal (read-file filename)))
