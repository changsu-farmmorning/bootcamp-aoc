(ns aoc2018_4
  (:require [util :refer [parse-input]]))
;; 파트 1
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 다음날 0시 5분~11분, 다다음날 0시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.


;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.

(defn minute-from-date
  "11-04 14:22 -> 22"
  [date]
  (->> (re-find #"\d+-\d+ (\d+):(\d+)" date)
       rest
       second
       Integer/parseInt))


(defn make-map-from-line
  "[1518-11-01 00:00] Guard #10 begins shift -> {:key Guard :id 10 :min 00}
  [1518-11-01 00:05] falls asleep -> {:key falls :id 0 :min 5}
  [1518-11-01 00:25] wakes up -> {:key wakes :id 0 min 25"
  [line]
  (->> (re-find #"\[\d+-(\d+-\d+ \d+:\d+)\] (\S+) (\S+)" line)
       ((fn [[_ date key id]]
          {(keyword date) {:key key
                           :id  (Integer/parseInt (or (first (rest (re-find #"#(\d+)" id))) "0"))
                           :min (minute-from-date date)}}))))

(defn get-minute-data
  "({:sleep 45, :wakeup 55} {:sleep 36, :wakeup 46} {:sleep 40, :wakeup 50})
  -> {36 1 37 1 ... 40 2 41 2 ... 45 3 ... 54 1}"
  [times]
  (->> (map (fn [times]
              (->> (for [minute (range (:sleep times) (:wakeup times))]
                     {minute 1})
                   (into (sorted-map))))
            times)
       (apply merge-with +)))

(defn sum-of-values
  "{36 1 37 2 38 3 39 4} -> 10 (1 + 2 + 3 + 4)"
  [m]
  (reduce (fn [acc [_ v]]
            (+ acc v))
          0
          m))

(defn key-of-max-values
  "{36 1 37 2 38 3 39 4} -> 39 (value: 4)"
  [maps]
  (first (sort-by second #(> %1 %2) maps)))

(defn multiply-id-minute
  "{:id 5 :minutes {36 1 37 2 .. 43 6}} -> 215 (5 * 43)"
  [{:keys [id minutes]}]
  (* id (first (key-of-max-values minutes))))

(comment
  (parse-input "resources/day4.txt")
  (def input '("[1518-11-01 00:00] Guard #10 begins shift"
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
                "[1518-11-05 00:55] wakes up"))             ;
  (def input (parse-input "resources/day4.txt"))

  (->> input
       (map make-map-from-line)
       (into (sorted-map))
       (reduce
         (fn [acc [_ {:keys [key id min]}]]
           (case key
             "Guard" (assoc acc :id id)
             "wakes" (update acc (:id acc) (fn [data]
                                             (conj (rest data) (assoc (first data) :wakeup min))))
             "falls" (update acc (:id acc) (fn [data]
                                             (conj (or data []) {:sleep min})))))
         {})
       (filter (fn [[k _]] (not= k :id)))
       (map (fn [[id times]] {:id id :minutes (get-minute-data times)}))
       (sort-by :minutes #(> (second (max-of-values %1)) (second (max-of-values %2))))
       first
       multiply-id-minute)

  (->> input
       (map make-map-from-line)
       (into (sorted-map))
       (reduce
         (fn [acc [_ {:keys [key id min]}]]
           (case key
             "Guard" (assoc acc :id id)
             "wakes" (update acc (:id acc) (fn [data]
                                             (conj (rest data) (assoc (first data) :wakeup min))))
             "falls" (update acc (:id acc) (fn [data]
                                             (conj (or data []) {:sleep min})))))
         {})
       (filter (fn [[k _]] (not= k :id)))
       (map (fn [[id times]] {:id id :minutes (get-minute-data times)}))
       (sort-by :minutes #(> (sum-of-values %1) (sum-of-values %2)))
       first
       multiply-id-minute))

;{:id 677,
;  :minutes {10 1,
;            11 1,
;            12 2, ...
;            }



