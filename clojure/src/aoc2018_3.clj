(ns aoc2018_3)


;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)


;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)

(defn parse-input
  ""
  [source]
  (map #(str %) (clojure.string/split-lines (slurp source))))

(defn get-data-by-pattern-from-str
  "주어진 문자 패턴으로 인풋을 인식하여 x, y 중 하나를 리턴하는 함수
  (\\d+)x(\\d+) 는 사이즈 정보를 나타내고
  (\\d+),(\\d+) 는 위치 정보를 나타낸다"
  [input pattern pos]
  (->> (re-find pattern input)
       (drop 1)
       (pos)
       (Integer/parseInt)))

(defn get-size
  "사이즈 정보 데이터에서 x|y 를 리턴하는 함수"
  [size x|y]
  (get-data-by-pattern-from-str size #"(\d+)x(\d+)" x|y))

(defn get-pos
  "위치 정보 데이터에서 x|y 를 리턴하는 함수"
  [pos x|y]
  (get-data-by-pattern-from-str pos #"(\d+),(\d+)" x|y))

(defn get-range
  "확인해야하는 가장 넒은 범위를 지정한다.
  이 범위 바깥의 점은 모두 Infinite area 를 가지게 된다."
  [[pos size] f]
  (let [x (get-pos pos f)
        x_size (get-size size f)]
    (range x (+ x x_size))))

(defn update-area-from-input
  "area 안의 각 점을 어떤 dot 이 차지하는지 확인하는 함수"
  [area [id & pos-size]]
  (->> (for [x (get-range pos-size first)
             y (get-range pos-size second)]
         (if (contains? area [x y])
           {[x y] (vec (flatten [(get area [x y]) id]))}
           {[x y] [id]}))
       (apply merge area)))

(defn find-overlap-map
  "해당 지점에 겹친 점들을 확인"
  [area]
  (for [[_ ids] area]
    (if (< 1 (count ids))
      [:overlap (set ids)]
      [:ids (set ids)])))

(defn find-not-overlap
  "겹치지 않은 점이 어디인지를 확인 Part2 의 정답"
  [area-map]
  (clojure.set/difference (:ids area-map) (:overlap area-map)))

(comment
  (let [{:keys [pos size]} {:pos  {:x 1
                                   :y 2}
                            :size {:x 3
                                   :y 4}}]
    (prn (:x pos) pos size))
  (def input '("#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2")) ;; sample
  (def input (parse-input "resources/day3.txt"))            ;; real-data
  (->> input
       (map #(rest (re-find #"#(\d+) @ (\d+,\d+): (\d+x\d+)" %)))
       (reduce update-area-from-input {})
       (filter (fn [[_ v]] (< 1 (count v))))
       count)
  (->> input
       (map #(rest (re-find #"#(\d+) @ (\d+,\d+): (\d+x\d+)" %)))
       (reduce update-area-from-input {})
       find-overlap-map
       (reduce (fn [acc [k v]]
                 (if (= k :overlap)
                   (merge acc {:overlap (clojure.set/union (get acc :overlap) v)})
                   (merge acc {:ids (clojure.set/union (get acc :ids) v)})))
               {:overlap #{} :ids #{}})
       find-not-overlap
       first))


