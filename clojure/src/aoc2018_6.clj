(ns aoc2018_6
  (:require [clojure.math :refer [round]]))

;; 파트 1
;; 입력 : 좌표의 쌍이 N개 주어짐

;; 1, 1
;; 1, 6
;; 8, 3
;; 3, 4
;; 5, 5
;; 8, 9

;; 각 점은 1 tick이 지날때 마다 상,하,좌,우로 증식함.


;;  ..........
;;  .A........
;;  ..........
;;  ........C.
;;  ...D......
;;  .....E....
;;  .B........
;;  ..........
;;  ..........
;;  ........F.


;;  aaaaa.cccc
;;  aAaaa.cccc
;;  aaaddecccc
;;  aadddeccCc
;;  ..dDdeeccc
;;  bb.deEeecc
;;  bBb.eeee..
;;  bbb.eeefff
;;  bbb.eeffff
;;  bbb.ffffFf


;; 여기서 . 으로 표기된 부분은 각 출발 지점으로부터 '같은 거리'에 있는 부분을 뜻함.
;; 맵 크기에는 제한이 없어 무한으로 뻗어나간다고 할 때, 가장 큰 유한한 면적의 크기를 반환 (part-1)


;; 파트 2
;; 안전(safe) 한 지역은 근원지'들'로부터의 맨하탄거리(Manhattan distance, 격자를 상하좌우로만 움직일때의 최단 거리)의 '합'이 N 미만인 지역임.

;;  ..........
;;  .A........
;;  ..........
;;  ...###..C.
;;  ..#D###...
;;  ..###E#...
;;  .B.###....
;;  ..........
;;  ..........
;;  ........F.

;; Distance to coordinate A: abs(4-1) + abs(3-1) =  5
;; Distance to coordinate B: abs(4-1) + abs(3-6) =  6
;; Distance to coordinate C: abs(4-8) + abs(3-3) =  4
;; Distance to coordinate D: abs(4-3) + abs(3-4) =  2
;; Distance to coordinate E: abs(4-5) + abs(3-5) =  3
;; Distance to coordinate F: abs(4-8) + abs(3-9) = 10
;; Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

;; N이 10000 미만인 안전한 지역의 사이즈를 구하시오.

(defn boundary-dots
  "주어진 점들중 가장자리에 있는 점들의 집합을 반환
  가장자리는 점을 기준으로 사분면을 나눴을 때 사분면 중 어느 한곳에는 점이 없는 경우이다"
  [dots dot]
  (nil? (and (not-empty (filter #(and
                                   (not= (:x %) (:x dot))
                                   (>= (:x %) (:x dot))
                                   (>= (:y %) (:y dot)))
                                dots))
             (not-empty (filter #(and
                                   (not= (:x %) (:x dot))
                                   (>= (:x %) (:x dot))
                                   (<= (:y %) (:y dot)))
                                dots))
             (not-empty (filter #(and
                                   (not= (:x %) (:x dot))
                                   (<= (:x %) (:x dot))
                                   (>= (:y %) (:y dot)))
                                dots))
             (not-empty (filter #(and
                                   (not= (:x %) (:x dot))
                                   (<= (:x %) (:x dot))
                                   (<= (:y %) (:y dot)))
                                dots)))))

(defn with-meta-data
  "주어진 데이터를 필요한 데이터들의 map 형태로 만든다"
  [dots]
  {:max           {:x (apply max (map :x dots)) :y (apply max (map :y dots))}
   :min           {:x (apply min (map :x dots)) :y (apply min (map :y dots))}
   :center        {:x (round (/ (apply + (map :x dots))
                                (count dots)))
                   :y (round (/ (apply + (map :y dots))
                                (count dots)))}
   :boundary-dots (set (filter #(boundary-dots dots %) dots))
   :dots          dots})

(defn parse-input
  "주어진 데이터를 점들의 집합으로 변환"
  [source]
  (let [extractor #(rest (re-find #"(\d+), (\d+)" %))
        input-split-by-newline (clojure.string/split-lines (slurp source))]
    (->> (map extractor input-split-by-newline)
         (map (fn [[x y]]
                {:x (parse-long x) :y (parse-long y)})))))

(defn distance-from-dot
  "두 점 사이의 거리를 구하는 함수
  d((x1 y1) (x2 y2)) = |x1 - x2| + |y1 - y2|"
  [from to]
  (let [distance-from-x (abs (- (:x from) (:x to)))
        distance-from-y (abs (- (:y from) (:y to)))]
    (+ distance-from-x distance-from-y)))


(defn check-duplicated-distance
  "내림차순으로 정렬된 맵 리스트에서 [{{:x x :y y} :distance d} ...] 앞의 두 점이 같은 거리를 가지고 있는지 판단"
  [dots]
  (if (= (:distance (first dots)) (:distance (second dots)))
    nil
    (first dots)))

(defn find-nearest-dot
  "현재 지점에서 가장 가까운 점을 반환.
  2개 이상의 가까운 지점이 있는 경우 nil 을 반환"
  [{:keys [x y dots]}]
  (->> (map (fn [dot]
              {:dot dot :distance (distance-from-dot {:x x :y y} dot)}) dots)
       (sort-by :distance <)
       check-duplicated-distance))

(defn make-area
  "확인할 영역을 생성"
  [d]
  (for [x (range (:x (:min d)) (:x (:max d)))
        y (range (:y (:min d)) (:y (:max d)))]
    {:x x :y y}))

(defn draw-area
  "확인해야할 각 점에서 가장 가까운 점들의 위치를 가진 맵을 생성
  {{:x :y} {:x :y}...}"
  [d]
  (let [dots (:dots d)]
    (assoc d :area (->> (make-area d)
                        (map #(vec [% (find-nearest-dot {:x (:x %) :y (:y %) :dots dots})]))
                        (filter second)
                        (into {})))))

(defn find-largest-area
  "가장 큰 넓이를 가지고 있으면서 boundary 가 아닌 점의 넓이를 반환"
  [d]
  (let [boundary-dots (:boundary-dots d)]
    (->> (:area d)
         (map #(hash-map (:dot (val %)) 1))
         (apply merge-with +)
         (remove #(boundary-dots (first %)))
         (apply max-key val)
         val)))

(defn sum-of-distance
  "해당 점에서 각 점까지의 모든 거리의 합을 반환"
  [{:keys [dots dot]}]
  (apply +
         (map
           #(distance-from-dot dot %)
           dots)))


(defn cal-safe-area
  "중심점을 기준으로 iter 만큼 떨어진 사각형에서 주어진 조건을 만족하는 (safe 한) 점의 개수를 반환"
  [iter limit {:keys [center dots]}]
  (+
    (->> (for [x (range (- (:x center) iter)
                        (+ (:x center) iter))]
           (sum-of-distance {:dots dots
                             :dot  {:x x
                                    :y (- (:y center) iter)}}))
         (filter #(> limit %))
         count)
    (->> (for [y (range (- (:y center) iter)
                        (+ (:y center) iter))]
           (sum-of-distance {:dots dots
                             :dot  {:x (+ (:x center) iter)
                                    :y y}}))
         (filter #(> limit %))
         count)
    (->> (for [x (range (inc (- (:x center) iter))
                        (inc (+ (:x center) iter)))]
           (sum-of-distance {:dots dots
                             :dot  {:x x
                                    :y (+ (:y center) iter)}}))
         (filter #(> limit %))
         count)
    (->> (for [y (range (inc (- (:y center) iter))
                        (inc (+ (:y center) iter)))]
           (sum-of-distance {:dots dots
                             :dot  {:x (- (:x center) iter)
                                    :y y}}))                ;; iterate + take-while 변경
         (filter #(> limit %))
         count)))

(defn find-safe-area
  "safe 한 지역의 넓이를 반환"
  [limit d]
  (->> (iterate inc 1)
       (take-while #(< 0 (cal-safe-area % limit d)))
       (map #(cal-safe-area % limit d))
       (apply + 1)))

(comment
  (->> (parse-input "resources/day6.txt")
       with-meta-data
       draw-area
       find-largest-area
       time)
  (->> (parse-input "resources/day6.txt")
       with-meta-data
       (find-safe-area 10000)))



