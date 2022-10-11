(ns aoc2018_7
  (:require [clojure.set]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn find-root
  ""
  [{:keys [done] :as m}]
  (->> (dissoc m :now-step :root :order :done :all-steps)
       (map (fn [[step dependencies]]
              (if (and
                    (nil? (step done))
                    (empty? (set/difference dependencies done)))
                step)))
       (keep identity)
       (into (sorted-set))))

(defn make-map-with-meta-data
  ""
  [m]
  (let [root (find-root m)
        first-step (first root)]
    (assoc m :now-step first-step
             :root root
             :all-steps (into #{} (keys m))
             :order (name first-step)
             :done #{first-step})))

(defn parse-input
  ""
  [source]
  (->> (str/split-lines (slurp source))
       (map #(re-find #"Step (\S) must be finished before step (\S) can begin." %))
       (map rest)
       (map #(hash-map (keyword (second %)) #{(keyword (first %))}
                       (keyword (first %)) #{}))
       (apply merge-with clojure.set/union)
       make-map-with-meta-data))

(defn take_step
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

(defn find-order
  ""
  [init-state]
  (->> (iterate take_step init-state)
       (drop-while #(not= (:done %) (:all-steps %)))
       first))

(comment
  (->> (parse-input "resources/day7_sample.txt")
       find-order)
  (->> (parse-input "resources/day7.txt")
       find-order)) =