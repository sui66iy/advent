
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
      [front (subs s 0 1)
       back (subs s 1)]
    (let [b (first back)]
      (cond
        (nil? b)
        (str front back)
        (reactable? (last front) b)
        (str (subs front 0 (- (count front) 1))
             (subs back 1))
        :else
        (recur (str front b)
               (subs back 1))))))

(defn react-until-fixed
  [s]
  (loop
      [prior s]
    (let
       [current (react prior)]
      (if (= prior current)
        current
        (recur current)))))
  
(def test-main "dabAcCaCBAcCcaDA")

(defn read-file
  [filename]
  (slurp filename))

(defn main
  [filename]
  (count (react-until-fixed (read-file filename))))
