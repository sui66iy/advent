
(require '[clojure.java.io :as io])
(require '[clojure.set :as set])

(defn add-next-step
  "Incrementally calculate the next-steps and prereqs maps from an instruction"
  [[next-steps prereqs] instruction]
  (let [match (re-matches #"Step (.+) must be finished before step (.+) can begin."
                          instruction)
        next-step (nth match 2)
        step (nth match 1)]
    [(update next-steps step #(conj % next-step))
     (update prereqs next-step #(conj % step))]))

(defn make-deps
  "Returns pair of [next-steps prereqs] maps.
  next-steps is the map of step -> steps that become ready.
  prereqs is the map of step -> steps that must already be ready."
  [instructions]
  (reduce add-next-step [{} {}] instructions))

(defn initial-ready
  "Return the initially ready set of steps in sorted order.
  Maintaining the sorted-set throughout the algorithm is crucial."
  [deps]
  (let [next-steps (first deps)
        children (set (flatten (vals next-steps)))
        all-steps (set (concat (keys next-steps) children))]
    (apply sorted-set (filter #(not (contains? children %)) all-steps))))

(defn step-is-ready?
  "Check to see if a step is ready by comparing its prerequisite steps
  to what steps have been done.  done should be a set."
  [deps done step]
  (let [prereqs (second deps)
        prereq-steps (prereqs step)]
    (every? #(contains? done %) prereq-steps)))

(defn next-ready
  "Return the next set of ready steps in sorted order"
  [deps ready current done]
  (let
      [next-steps (first deps)]
    (set/union (disj ready current)
               (apply sorted-set
                      (filter #(step-is-ready? deps done %)
                              (next-steps current))))))
             
(defn compute-order
  "Iterate over the dependency graph, returning the list of done steps
  in the correct order."
  [deps]
  (loop [ready (initial-ready deps)
         done []]
    (if (empty? ready)
      done
      (let [current (first ready)
            done (conj done current)]
        (recur (next-ready deps ready current (set done))
               done)))))

(def test-main ["Step C must be finished before step A can begin."
                "Step C must be finished before step F can begin."
                "Step A must be finished before step B can begin."
                "Step A must be finished before step D can begin."
                "Step B must be finished before step E can begin."
                "Step D must be finished before step E can begin."
                "Step F must be finished before step E can begin."])

(defn main
  [filename]
  (with-open [rdr (io/reader filename)]  
    (apply str (compute-order (make-deps (line-seq rdr))))))

;; Part 2

(def main-step-time 60)

(def test-step-time 0)

(def step-time main-step-time)

(def test-n-workers 2)

(def main-n-workers 5)

(def n-workers main-n-workers)

(def step-times (into {} (map vector
                              (map str "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
                              (range 1 27))))

(defn get-ready
  [step]
  (if (nil? step)
    nil
    [step (+ (step-times step) step-time)]))

(defn update-workers
  [deps
   the-workers
   the-ready
   the-done]
  (loop [idx 0
         workers the-workers
         ready the-ready
         done the-done]
    (if (= idx (count workers))
      [workers ready done]
      (let [w (nth workers idx)]
        (cond (nil? w) ; worker is empty; try to get some work for it!
              (recur (+ idx 1)
                     (assoc workers idx (get-ready (first ready)))
                     (disj ready (first ready))
                     done)
              (= (second w) 1) ; worker is done; add work to done and get some new work!
              (let [new-done (conj done (first w))
                    new-ready (next-ready deps ready (first w) (set new-done))]
                (recur (+ idx 1)
                       (assoc workers idx (get-ready (first new-ready)))
                       (disj new-ready (first new-ready))                     
                       new-done))
              :else ; work in progress; update the work
              (recur (+ idx 1)
                     (assoc workers idx [(first w) (- (second w) 1)])
                     ready
                     done))))))

(defn compute-order2
  [deps]
  (loop [ready (initial-ready deps)
         done []
         sec -1
         workers (into [] (take n-workers (repeat nil)))]
    (if (and (empty? ready)
             (every? nil? (set workers)))
      [sec done]
      (let [[updated-workers updated-ready updated-done]
            (update-workers deps workers ready done)]
        (recur updated-ready
               updated-done
               (+ sec 1)
               updated-workers)))))
          
(defn test2 [] (compute-order2 (make-deps test-main)))

(defn main2
  [filename]
  (with-open [rdr (io/reader filename)]
    (let [[secs steps] (compute-order2 (make-deps (line-seq rdr)))]
      [secs (apply str steps)])))
