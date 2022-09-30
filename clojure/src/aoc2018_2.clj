(ns aoc2018-2)

;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12


;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.


;; #################################
;; ###        Refactoring        ###
;; #################################

(defn parse-input
  "Parse input data, it makes txt file to string map.
  input: file name (resources/day2.txt)
  output: (\"aaaa\" \"bbbb\" ..)"
  [input]
  (map #(str %) (clojure.string/split-lines (slurp input))))

(defn how-many-appear-in-string
  "input: \"bababc\"
  output: {:a 2 :b 3 :c 1}"
  [input]
  (->>
    (char-array input)
    (reduce #(update %1 (keyword (str %2)) (fnil + 0) 1)
            {})))


(defn check-n-times-in-coll
  "input: {:a 1 :b 4 :c 3 :d 4 :e 5} 4
  output: true (:b :d)"
  [n-times coll]
  (->>
    (vals coll)
    (filter #{n-times})
    count
    (< 0)))

(defn check-two-or-three-times-in-coll
  "input: {:a 2 :b 3 :c 3 :d 5}
  output: {:2 true, :3 true}"
  [coll]
  (->>
    {:2 (check-n-times-in-coll 2 coll) :3 (check-n-times-in-coll 3 coll)}))

(defn boolean-to-int
  ""
  [input]
  (get {false 0 true 1} input))

(defn merge-two-or-three-map
  ""
  [exist new-map]
  (assoc exist
    :2 (+ (get exist :2) (boolean-to-int (get new-map :2)))
    :3 (+ (get exist :3) (boolean-to-int (get new-map :3)))))

(defn multiple-map-element
  ""
  [map]
  (* (get map :2) (get map :3)))

(defn find-same-char-between-two-string
  ""
  [str1 str2]
  (->> (map #(if (= %1 %2)
               %1
               nil) str1 str2)
       (remove nil?)
       (apply str)))

(defn part1-solver
  ""
  [input]
  (->> input
       (map frequencies)
       (map check-two-or-three-times-in-coll)
       (reduce merge-two-or-three-map {:2 0 :3 0})
       multiple-map-element))

(defn part2-solver
  ""
  [input]
  (->> (for [first input
             second input
             :when (not= first second)
             :let [same-character (find-same-char-between-two-string first second)]]
         same-character)
       (filter #(= (count %) (- (count (first input)) 1)))
       first))

(comment
  (def input (parse-input "resources/day2.txt"))
  (part1-solver input)
  (part2-solver input))
