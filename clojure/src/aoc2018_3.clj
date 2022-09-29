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

(defn get-data-from-str
  "input: \"#1 @ 1,3: 4x4\" \"#2 @ 3,1: 4x4\"
  output: (\"1\" \"1,3\" \"4x4\")"
  [input-str]
  (drop 1 (re-find #"#(\d+) @ (\d+,\d+): (\d+x\d+)" input-str)))

(defn get-data-by-pattern-from-str
  ""
  [input pattern pos]
  (->>
    (re-find pattern input)
    (drop 1)
    (pos)
    (Integer/parseInt)))

(defn get-size
  ""
  [size x-or-y]
  (get-data-by-pattern-from-str size #"(\d)x(\d)" x-or-y))

(defn get-pos
  ""
  [pos x-or-y]
  (get-data-by-pattern-from-str pos #"(\d),(\d)" x-or-y))

(defn get-range
  ""
  [pos size f]
  (let [x (get-pos pos f)
        x_size (get-size size f)]
    (range x (+ x x_size))))

(defn update-area-from-input
  ""
  [area [id, pos, size]]
  (->>
    (for [x (get-range pos size first)
          y (get-range pos size second)]
      (if (contains? area [x, y])
        {[x, y] (vec (flatten [(get area [x, y]) id]))}
        {[x, y] [id]}))
    (apply merge)
    (merge area)))

(comment
  (def input '("#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"))
  (def input (parse-input "resources/day3.txt"))
  (->>
    input
    (map get-data-from-str)
    (reduce update-area-from-input {})
    (reduce #(if (< 1 (count (second %2)))
               (+ %1 1)
               %1) 0)))

