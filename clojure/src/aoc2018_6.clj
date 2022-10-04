(ns aoc2018_6)

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

(defn get-meta-data
  ""
  [dots]
  {:max  {:x (apply max (map :x dots)) :y (apply max (map :y dots))}
   :min  {:x (apply min (map :x dots)) :y (apply min (map :y dots))}
   :dots dots})

(defn parse-input
  ""
  [source]
  (->> (map #(rest (re-find #"(\d+), (\d+)" %))
            (clojure.string/split-lines (slurp source)))
       (map (fn [[x y]]
              {:x (read-string x) :y (read-string y)}))))

(defn distance-from-dot
  ""
  [from to]
  (+ (abs (- (:x from) (:x to))) (abs (- (:y from) (:y to)))))

(defn check-duplicated-distance
  ""
  [dots]
  (if (= (:distance (first dots)) (:distance (second dots)))
    nil
    (first dots)))

(defn get-nearest-dot
  ""
  [{:keys [x y dots]}]
  (->> (map (fn [dot]
              {:dot dot :distance (distance-from-dot {:x x :y y} dot)}) dots)
       (sort-by :distance <)
       check-duplicated-distance))

(defn draw-area
  ""
  [d]
  (assoc d :area (for [x (range (:x (:min d)) (:x (:max d)))
                       y (range (:y (:min d)) (:y (:max d)))]
                   {{:x x :y y} (get-nearest-dot {:x x :y y :dots (:dots d)})})))

(defn find-largest-area
  ""
  [{:keys [dot]}]
  ())

(defn find-have-largest-area-dot
  ""
  [d]
  ())

(comment
  (def input [{:x 1, :y 1} {:x 1, :y 6} {:x 8, :y 3} {:x 3, :y 4} {:x 5, :y 5} {:x 8, :y 9}])
  (parse-input "resources/day6.txt")
  (->> input
       get-meta-data
       draw-area
       find-have-largest-area-dot)
  (= {:x 1 :y 2} {:x 1 :y 3 :z 3}))



