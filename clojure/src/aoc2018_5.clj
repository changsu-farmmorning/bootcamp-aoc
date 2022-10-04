(ns aoc2018_5)                                              ;; java import
;; 파트 1
;; 입력: dabAcCaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.

;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.

(defn change-capitals
  "a -> A
  A -> a"
  [s]
  (cond
    (Character/isUpperCase s) (Character/toLowerCase s)
    (Character/isLowerCase s) (Character/toUpperCase s)))

(defn react-polymer
  "aAbBcC -> ()
  aBcD -> (\\a \\B \\c \\D)"
  [p]
  (reduce (fn [acc val]
            (if (= (first acc) (change-capitals val))
              (rest acc)
              (conj acc val)))
          ()
          p))

(defn get-alphabet-pattern
  "(#\"A|a\" ... #\"Z|z\")"
  []
  (->> (map #(str (char %) "|" (char (+ % 32))) (range 65 91))
       (map re-pattern)))

(defn get-all-replace-one-unit-polymer
  "dabAcCaCBAcCcaDA
  ->
   ([\\d \\b \\c \\C \\C \\B \\c \\C \\c \\D]
   [\\d \\a \\A \\c \\C \\a \\C \\A \\c \\C \\c \\a \\D \\A]
   [\\d \\a \\b \\A \\a \\B \\A \\a \\D \\A]
   [\\a \\b \\A \\c \\C \\a \\C \\B \\A \\c \\C \\c \\a \\A]
   [\\d \\a \\b \\A \\c \\C \\a \\C \\B \\A \\c \\C \\c \\a \\D \\A] ...)"
  [p]
  (for [pattern (get-alphabet-pattern)]
    (into [] (clojure.string/replace p pattern ""))))

(comment
  (get-alphabet-pattern)
  (def input "dabAcCaCBAcCcaDA")
  (def input (slurp "resources/day5.txt"))
  (->> input
       (into [])
       react-polymer
       count)

  (->> input
       get-all-replace-one-unit-polymer
       (map react-polymer)
       (map count)
       (apply min)))
