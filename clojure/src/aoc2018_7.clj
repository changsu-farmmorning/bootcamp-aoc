(ns aoc2018_7
  (:require [clojure.set]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn find-root
  ""
  [{:keys [done] :as m}]
  (->> (dissoc m :now-step :root :order :done :all-steps :workers :time)
       (map (fn [[step dependencies]]
              (if (and
                    (nil? (step done))
                    (empty? (set/difference dependencies done)))
                step)))
       (keep identity)
       (into (sorted-set))))

(defn make-map-with-meta-data-part-1
  ""
  [m]
  (let [root (find-root m)
        first-step (first root)]
    (assoc m :now-step first-step
             :root root
             :all-steps (into #{} (keys m))
             :order (name first-step)
             :done #{first-step})))

(defn make-map-with-meta-data-part-2
  ""
  [m]
  (let [root (find-root m)
        first-step (first root)]
    (assoc m :now-step first-step
             :root root
             :all-steps (into #{} (keys m))
             :order (name first-step)
             :done #{}
             :time 0
             :workers [{:step     nil
                        :duration 0}
                       {:step     nil
                        :duration 0}
                       {:step     nil
                        :duration 0}
                       {:step     nil
                        :duration 0}
                       {:step     nil
                        :duration 0}])))

(defn parse-input
  ""
  [source]
  (->> (str/split-lines (slurp source))
       (map #(re-find #"Step (\S) must be finished before step (\S) can begin." %))
       (map rest)
       (map #(hash-map (keyword (second %)) #{(keyword (first %))}
                       (keyword (first %)) #{}))
       (apply merge-with clojure.set/union)))

(defn take-step
  ""
  [{:keys [order] :as state}]
  (let [new-root (find-root state)
        new-step (first new-root)
        new-order (str order (if (some? new-step)
                               (name new-step)
                               ""))
        new-done (conj (:done state) new-step)]
    (assoc state :root new-root
                 :order new-order
                 :now-step new-step
                 :done new-done)))

(defn take-step-part-2
  ""
  [state]
  (let [new-root (find-root state)]
    (assoc state :root new-root)))



(defn find-order
  ""
  [init-state]
  (->> (iterate take-step init-state)
       (drop-while #(not= (:done %) (:all-steps %)))
       first))

(defn required-step-duration
  ""
  [step]
  (-> step
      name
      char-array
      first
      int
      (- 4)))

(defn find-finished-job
  ""
  [workers]
  (->> workers
       (filter (fn [{:keys [step duration]}]
                 (if (nil? step)
                   false
                   (= duration
                      (required-step-duration step)))))
       (map #(:step %))
       (into #{})))

(defn release-job
  "[{:step :B :duration 4}]
  [{:step nil :duration 0}]"
  [workers finished-jobs]
  (->> workers
       (map (fn [{:keys [step] :as worker}]
              (if (or (nil? step)
                      (nil? (step finished-jobs)))
                worker
                (assoc worker :step nil
                              :duration 0))))
       (into [])))

(defn check-completed-workers
  ""
  [{:keys [workers done] :as state}]
  (let [finished-jobs (find-finished-job workers)
        new-workers (release-job workers finished-jobs)]
    (if (empty? finished-jobs)
      state
      (->> (assoc state :workers new-workers
                        :done (set/union done finished-jobs))
           take-step-part-2))))

(defn working-jobs
  ""
  [workers]
  (->> workers
       (map #(:step %))
       (keep identity)
       (into #{})))

(defn empty-worker-idx
  ""
  [workers]
  (->> workers
       (map-indexed vector)
       (filter #(->> %
                     second
                     :step
                     nil?))
       ffirst))

(defn assign-job
  ""
  [{:keys [jobs workers] :as origin}]
  (let [can-assign-jobs (->> workers
                             working-jobs
                             (set/difference jobs)
                             (into (sorted-set)))
        not-assign-job (first can-assign-jobs)
        empty-worker-idx (empty-worker-idx workers)]
    (if (nil? empty-worker-idx)
      origin
      {:workers (assoc workers empty-worker-idx
                               {:step not-assign-job :duration 0})
       :jobs    (set/difference can-assign-jobs #{not-assign-job})})))


(defn can-working?
  ""
  [workers]
  (->> workers
       empty-worker-idx
       some?))

(defn assign-new-jobs
  ""
  [{:keys [workers root] :as state}]
  (assoc state :workers (->> (iterate assign-job {:workers workers :jobs root})
                             (drop-while #(and (can-working? (:workers %))
                                               (not-empty (:jobs %))))
                             first
                             :workers)))

(defn inc-time
  ""
  [{:keys [workers] :as state}]
  (-> state
      (assoc :workers (->> workers
                           (map #(if (some? (:step %))
                                   (update % :duration inc)
                                   %))
                           (into [])))
      (update :time inc)))

(defn take-one-second
  ""
  [state]
  (->> state
       check-completed-workers
       assign-new-jobs
       inc-time))


(defn find-take-time
  ""
  [init-state]
  (->> (iterate take-one-second init-state)
       (drop-while #(not= (:done %) (:all-steps %)))
       first))

(comment
  (->> (parse-input "resources/day7_sample.txt")
       make-map-with-meta-data-part-1
       find-order)
  (->> (parse-input "resources/day7.txt")
       make-map-with-meta-data-part-1
       find-order)
  (->> (parse-input "resources/day7.txt")
       make-map-with-meta-data-part-2
       assign-new-jobs
       find-take-time))