
(require '[clojure.string :as string])

(defn reactable?
  [x y]
  (and (= (string/lower-case x)
          (string/lower-case y))
       (or (and (Character/isUpperCase x)
                (Character/isLowerCase y))
           (and (Character/isLowerCase x)
                (Character/isUpperCase y)))))

(defn react
  [s]
  (loop
      [front (subvec s 0 1)
       back (subvec s 1)]
    (let [b (first back)]
      (cond
        (nil? b)
        front
        (reactable? (last front) b)
        (into (subvec front 0 (- (count front) 1))
              (subvec back 1))
        :else
        (recur (conj front b)
               (subvec back 1))))))

(defn react-until-fixed
  [s]
  (loop
      [prior s]
    (let
        [current (react prior)]
      (println (count current))
      (if (= (count prior) (count current))
        current
        (recur current)))))
  
(def test-main (into [] "dabAcCaCBAcCcaDA"))

(defn read-file
  [filename]
  (into [] (slurp filename)))

(defn main
  [filename]
  (count (react-until-fixed (read-file filename))))
