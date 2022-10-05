(ns aoc2020_1)

;; 파트 1
;; 입력: 숫자 리스트가 주어짐

;; 1721
;; 979
;; 366
;; 299
;; 675
;; 1456

;; 합이 2020 이 되는 두 숫자를 찾아 두 값의 곱을 리턴하시오.
;; 1721 + 299 = 2020, 1721 * 299 = 514579


;; 파트 2
;; 입력: 숫자 리스트가 주어짐

;; 합이 2020 이 되는 세 숫자를 찾아 세 값의 곱을 리턴하시오.
;; 979 + 366 + 675 = 2020, 979 * 366 * 675 = 69596112


(defn parse-input
  ""
  [source]
  (->> (map #(Integer/parseInt %) (clojure.string/split-lines (slurp source)))))

(defn find-two-entries
  ""
  [target m]
  (->> (for [entry1 m
             entry2 m
             :when (=
                     target
                     (+ entry1 entry2))]
         [entry1 entry2])
       first
       (apply *)))

(defn find-three-entries
  ""
  [target m]
  (->> (for [entry1 m
             entry2 m
             entry3 m
             :when (=
                     target
                     (+ entry1 entry2 entry3))]
         [entry1 entry2 entry3])
       first
       (apply *)))


(comment
  (def input '(1721
                979
                366
                299
                675
                1456))
  (->> (parse-input "resources/2020_day1.txt")
       (find-two-entries 2020))
  (->> (parse-input "resources/2020_day1.txt")
       (find-three-entries 2020)))



