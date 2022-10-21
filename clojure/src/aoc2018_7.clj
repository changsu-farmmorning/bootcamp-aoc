(ns aoc2018_7
  (:require [clojure.set]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn find-root
  "현재 상황에서 디펜던시가 없는 step 들을 찾아 알파벳 순서대로 정렬한다."
  [{:keys [done] :as m}]
  (->> (dissoc m :now-step :root :order :done :all-steps :workers :time)
       (map (fn [[step dependencies]]
              (when (and
                      (nil? (step done))
                      (empty? (set/difference dependencies done)))
                step)))
       (keep identity)
       (into (sorted-set))))

(defn make-map-with-meta-data-part-1
  "part1 을 풀기 위한 meta 데이터를 붙인 최초 state 를 리턴한다"
  [m]
  (let [root (find-root m)
        first-step (first root)]
    (assoc m :now-step first-step
             :root root
             :all-steps (into #{} (keys m))
             :order (name first-step)
             :done #{first-step})))

(defn make-map-with-meta-data-part-2
  "part2 를 풀기 위한 meta 데이터를 붙인 최초 state 를 리턴한다"
  [num-of-workers m]
  (let [root (find-root m)]
    (assoc m :root root
             :all-steps (into #{} (keys m))
             :done #{}
             :time -1
             :workers (->> {:step nil :duration 0}
                           (repeat num-of-workers)
                           (into [])))))

(defn parse-input
  "데이터를 파싱하여 map 형태로 가공한다"
  [source]
  (->> (str/split-lines (slurp source))
       (map #(re-find #"Step (\S) must be finished before step (\S) can begin." %))
       (map rest)
       (map #(hash-map (keyword (second %)) #{(keyword (first %))}
                       (keyword (first %)) #{}))
       (apply merge-with clojure.set/union)))

(defn take-step
  "다음 스탭을 찾는다.
  새로운 루트를 찾고 루트들 중에 가장 첫번째 step 을 다음 스탭으로 판단한다.
  order 을 붙이고 작업이 종료가 되었는지 확인한다. "
  [{:keys [order] :as state}]
  (let [new-root (find-root state)
        new-step (first new-root)
        new-order (if (some? new-step)
                    (str order (name new-step))
                    order)
        new-done (conj (:done state) new-step)]
    (assoc state :root new-root
                 :order new-order
                 :now-step new-step
                 :done new-done)))

(defn take-step-part-2
  "작업이 모두 끝났을 때 다음 작업을 찾기 위해 ROOT 를 탐색하는 함수이다.
  part2 에서 사용된다"
  [state]
  (let [new-root (find-root state)]
    (assoc state :root new-root)))



(defn find-order
  "모든 작업을 완료하기 전까지 다음 step 을 계속 찾는다"
  [init-state]
  (->> (iterate take-step init-state)
       (drop-while #(not= (:done %) (:all-steps %)))
       first))

(defn required-step-duration
  "A -> 61 로 변경하는 함수.\A 가 아스키코드로 65 라는 점을 이용"
  [step]
  (-> step
      name
      char-array
      first
      int
      (- 4)))

(defn find-finished-job
  "진행중인 작업 중 완료된 작업을 set 형태로 리턴. 종료 조건은 A 61초, B 62초.. 이다"
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
  [{:step nil :duration 0}]
  작업을 완료 처리하여 작업없음 상태로 변경하는 함수"
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
  "if job finished, take next root"
  [{:keys [workers done] :as state}]
  (let [finished-jobs (find-finished-job workers)
        new-workers (release-job workers finished-jobs)]
    (if (empty? finished-jobs)
      state
      (->> (assoc state :workers new-workers
                        :done (set/union done finished-jobs))
           take-step-part-2))))

(defn working-jobs
  "현재 작업을 진행중인 작업자를 set 형태로 반환"
  [workers]
  (->> workers
       (map #(:step %))
       (keep identity)
       (into #{})))

(defn empty-worker-idx
  "작업을 하지 않고 있는 작업자들 중 가장 Idx 가 낮은 작업자의 idx 를 반환"
  [workers]
  (->> workers
       (map-indexed vector)
       (filter #(->> %
                     second
                     :step
                     nil?))
       ffirst))

(defn assign-job
  "할당해야 하는 작업 목록에서 하나를 뽑아 작업을 하지 않고 있는 작업자에게 할당"
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
  "추가 작업이 가능한 상태인지 여부를 확인"
  [workers]
  (->> workers
       empty-worker-idx
       some?))


(defn assign-new-jobs
  "작업 목록에서 하나씩 뽑아 작업자들에게 할당
  모든 작업자에게 일이 분배되어 더이상 일을 할수 없거나 모두 할당된 경우 잘 할당되었다고 판단"
  [{:keys [workers root] :as state}]
  (assoc state :workers (->> (iterate assign-job {:workers workers :jobs root})
                             (drop-while #(and (can-working? (:workers %))
                                               (not-empty (:jobs %))))
                             first
                             :workers)))

(defn inc-time
  "시간을 1초 증가시키는 함수
  각 작업자들의 시간을 올리고 전체 시간을 증가시키는 함수"
  [{:keys [workers] :as state}]
  (-> state
      (assoc :workers (->> workers
                           (map #(if (some? (:step %))
                                   (update % :duration inc)
                                   %))
                           (into [])))
      (update :time inc)))

(defn take-one-second
  "완료된 작업이 있는지 확인하고, 추가 작업을 할당하고, 전체 time 을 1초 증가시킨다"
  [state]
  (->> state
       check-completed-workers
       assign-new-jobs
       inc-time))

(defn find-take-time
  "모든 작업이 완료되기 전까지 계속 1초씩 진행시킨다"
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
       (make-map-with-meta-data-part-2 2)
       assign-new-jobs
       find-take-time)
  (->> (parse-input "resources/day7.txt")
       (make-map-with-meta-data-part-2 5)
       assign-new-jobs
       find-take-time))