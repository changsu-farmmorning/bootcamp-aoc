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
  (->> (map parse-long (clojure.string/split-lines (slurp source)))))

(defn find-two-entries
  ""
  [target m]
  (let [with-idx (map-indexed vector m)]
    (->> (for [[idx1 val1] with-idx
               [idx2 val2] with-idx
               :when (and
                       (= target (+ val1 val2))
                       (not= idx1 idx2))]
           [val1 val2])
         first
         (apply *))))

(defn find-three-entries
  ""
  [target m]
  (let [with-idx (map-indexed vector m)]
    (->> (for [[idx1 val1] with-idx
               [idx2 val2] with-idx
               [idx3 val3] with-idx
               :when (and
                       (= target (+ val1 val2 vla3))
                       (not= idx1 idx2)
                       (not= idx1 idx3)
                       (not= idx2 idx3))]
           [val1 val2 val3])
         first
         (apply *))))


(comment
  (def input '(1721
                979
                366
                299
                675
                1456))
  (for [a (map-indexed vector input)]
    (second a))
  (->> (parse-input "resources/2020_day1.txt")
       (find-two-entries 2020))
  (->> (parse-input "resources/2020_day1.txt")
       (find-three-entries 2020)))

;; :req-un :req 차이점 ?
;; defrecord ?

;; spec -> 클래스 or 팩토리 패턴
;; spec merge -> 상속
;; multi spec -> 다형성
;; using for validation -> 데코레이터 패





