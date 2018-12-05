
(require '[clojure.java.io :as io])

(defrecord Guard [id sleep-minutes])

(defn get-minute
  [s]
  (Integer. (second (re-find #":(\d\d)\]" s))))

(defn get-guard-id
  [s]
  (Integer. (second (re-find #"Guard #(\d+) " s))))

(defn shift-start?
  [s]
  (not (nil? (re-find #"begins shift" s))))

(defn wakes-up?
  [s]
  (not (nil? (re-find #"wakes up" s))))

(defn falls-asleep?
  [s]
  (not (nil? (re-find #"falls asleep" s))))

(defn get-guard
  [id guards]
  (if (nil? (guards id))
    (Guard. id {})
    (guards id)))

(defn inc-nil
  [x]
  (if (nil? x)
    1
    (inc x)))

(defn update-guard
  [guard sleep-minute wake-minute]
  (assoc guard :sleep-minutes
         (reduce (fn [m minute]
                   (update m minute inc-nil))
                 (:sleep-minutes guard)
                 (range sleep-minute wake-minute))))

(defn total-sleep-minutes
  [guard]
  (reduce + (vals (:sleep-minutes guard))))

(defn most-sleep-minute
  [guard]
  (first (first (sort-by val > (:sleep-minutes guard)))))

(defn most-sleep-count
  [guard]
  (second (first (sort-by val > (:sleep-minutes guard)))))

(defn get-guards
  [the-records]
  (loop
      [guards {}
       current-guard nil
       prior-minute nil
       records (sort the-records)]
    (let
        [record (first records)]
      (if (nil? record)
        (filter #(not (empty? (:sleep-minutes (val %)))) guards)
        (let [current-minute (get-minute record)]
          (cond (shift-start? record)
                (let [gid (get-guard-id record)
                      g (get-guard gid guards)]
                  (recur (assoc guards gid g)
                         g
                         current-minute
                         (next records)))
                (falls-asleep? record)
                (recur guards
                       current-guard
                       current-minute
                       (next records))
                (wakes-up? record)
                (let [gid (:id current-guard)
                      g (update-guard (get-guard gid guards)
                                      prior-minute current-minute)]
                  (recur (assoc guards gid g)
                         g
                         current-minute
                         (next records)))))))))

(defn sleepiest-guard
  [guards]
  (second (first (sort-by #(total-sleep-minutes (val %)) > guards))))

(defn frequently-sleepy-guard
  [guards]
  (second (first (sort-by #(most-sleep-count (val %)) > guards))))

(defn strategy1
  [records]
  (let [guards (get-guards records)
        g (sleepiest-guard guards)]
    (* (:id g) (most-sleep-minute g))))

(defn strategy2
  [records]
  (let [guards (get-guards records)
        g (frequently-sleepy-guard guards)]
    (* (:id g) (most-sleep-minute g))))

(defn test-main
  []
  (let [records ["[1518-11-01 00:00] Guard #10 begins shift"
                 "[1518-11-01 00:05] falls asleep"
                 "[1518-11-01 00:25] wakes up"
                 "[1518-11-01 00:30] falls asleep"
                 "[1518-11-01 00:55] wakes up"
                 "[1518-11-01 23:58] Guard #99 begins shift"
                 "[1518-11-02 00:40] falls asleep"
                 "[1518-11-02 00:50] wakes up"
                 "[1518-11-03 00:05] Guard #10 begins shift"
                 "[1518-11-03 00:24] falls asleep"
                 "[1518-11-03 00:29] wakes up"
                 "[1518-11-04 00:02] Guard #99 begins shift"
                 "[1518-11-04 00:36] falls asleep"
                 "[1518-11-04 00:46] wakes up"
                 "[1518-11-05 00:03] Guard #99 begins shift"
                 "[1518-11-05 00:45] falls asleep"
                 "[1518-11-05 00:55] wakes up"]]
    (strategy2 records)))

(defn main1
  [filename]
  (with-open [rdr (io/reader filename)]
    (strategy1 (line-seq rdr))))

(defn main2
  [filename]
  (with-open [rdr (io/reader filename)]
    (strategy2 (line-seq rdr))))
  
      
                               



