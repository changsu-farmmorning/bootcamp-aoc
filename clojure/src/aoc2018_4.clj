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

;; {id [(s_time, e_time) ...]}
;; sum(e_time, s_time) 이 가장 높은 id 하나만 추출
;; (id, [(s_time, e_time)...])
;; (id, {min frequencies}) 각 분마다의 출현빈도를 가지는 맵을 작성
;; (id, min) 가장 높은 출현빈도를 가지는 min 을 추출
;; id * min 리턴

;; clj-time 라이브러리 쓰는법? - 시간 관련 함수
;; 전체적인 방향성?
;; 사용하면 좋을 함수들?

(defn guard-input-to-map
  ""
  [[_ _ date time id]]
  {:id id :date date :time time :type "begins shift"})

(defn next-date
  ""
  [date]
  (->> (re-find #"(\d+)\-(\d+)" date)
       (rest)
       ((fn [[month day]]
          (let [day-int (Integer/parseInt day)]
            (str month "-"
                 (if (> 10 (+ day-int 1))
                   (str "0" (+ day-int 1))
                   (str (+ day-int 1)))))))))



(defn get-minute
  ""
  [time]
  (->> (re-find #"(\d+):(\d+)" time)
       (rest)
       ((fn [[_ min]]
          min))
       (Integer/parseInt)))


(defn check-work-early
  ""
  [{:keys [date time]}]
  (->> (re-find #"(\d+):(\d+)" time)
       (rest)
       ((fn [[hour _]]
          (if (= hour "23")
            (next-date date)
            date)))))

(defn cc
  [a b c]
  (println a))

(comment
  (get-min "10:04")
  (str "a" (str 24))
  (next-date "10-10")
  (->> ("10-11" "23" "59")
       (cc))
  (check-work-early {:date "10-11" :time "00:59"})

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
                "[1518-11-05 00:55] wakes up"))
  (->> input
       (map (fn [input-str]
              (let [guard-input (re-find #"\[(\d+)-(\d+-\d+) (\d+:\d+)\] Guard #(\d+) begins shift" input-str)
                    sleep-input (re-find #"\[(\d+)-(\d+-\d+) (\d+:\d+)\] falls asleep" input-str)
                    wakeup-input (re-find #"\[(\d+)-(\d+-\d+) (\d+:\d+)\] wakes up" input-str)]
                (cond
                  (some? guard-input) ((fn [[_ _ date time id]]
                                         {:id id :date date :time time :type "begins shift"}) guard-input)
                  (some? sleep-input) ((fn [[_ _ date time]]
                                         {:date date :time time :type "sleep"}) sleep-input)
                  (some? wakeup-input) ((fn [[_ _ date time]]
                                          {:date date :time time :type "wakeup"}) wakeup-input)
                  :else nil))))
       (reduce (fn [acc {:keys [id type date time]}]
                 (case type
                   "begins shift" (assoc acc
                                    (keyword (check-work-early {:date date :time time}))
                                    {:id id :sleep sorted-set :wakeup sorted-set})
                   "sleep" (update acc (keyword date) (fn [data]
                                                        (assoc data
                                                          :sleep
                                                          ((:sleep data sorted-set) (get-minute time)))))
                   "wakeup" acc))
               {}))
  (->> {:id 1 :date "10-11" :time "22:33" :type "begin"}
       ((fn [{:keys [id type date time]}]
          (println date))))
  (get {:1 3 2 4} 2)
  (conj {})
  (conj (sorted-set 1) 4)
  (sorted-set 12344 123 555 111111 1)
  (cond
    (some? nil) true
    (some? "") false)
  (->> "[1518-11-05 00:03]"
       (re-find #"\[\d+-(\d+-\d+) (\d+:\d+)\]")
       (some?)))


