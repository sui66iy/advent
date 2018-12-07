
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
