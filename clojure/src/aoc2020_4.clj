(ns aoc2020_4
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))


;; 파트 1

;; 여권정보에 필수적인 필드는 아래와 같다. cid 는 없어도 무방하지만 다른 모든 필드의 값이 정확히 있어야 유효한 여권이라고 판단한다.
;; 하나의 필드는 : 로 연결되어 있으며 각 필드들은 공백이나 개행으로 분리되어있다.
;; 새로운 여권정보는 빈 라인으로 구분된다.

;; byr (Birth Year)
;; iyr (Issue Year)
;; eyr (Expiration Year)
;; hgt (Height)
;; hcl (Hair Color)
;; ecl (Eye Color)
;; pid (Passport ID)
;; cid (Country ID)

;; input

;; ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
;; byr:1937 iyr:2017 cid:147 hgt:183cm
;;
;; iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
;; hcl:#cfa07d byr:1929
;;
;; hcl:#ae17e1 iyr:2013
;; eyr:2024
;; ecl:brn pid:760753108 byr:1931
;; hgt:179cm
;;
;; hcl:#cfa07d eyr:2025 pid:166559648
;; iyr:2011 ecl:brn hgt:59in

;; 위와 같은 인풋에서
;; 첫번째 여권은 모든 필드가 있으므로 유효하다.
;; 두번째 여권은 cid 필드가 없으나 cid 필드는 optional 이므로 유효하다.
;; 세번째 여권은 cid, byr 필드가 없어서 유효하지 않다.

;; 따라서 유효한 여권의 총 개수는 2이다.

;; 주어진 인풋에서 유효한 여권의 개수를 리턴하시오.

;; part1
;(s/def :passport/byr string?)
;(s/def :passport/iyr string?)
;(s/def :passport/eyr string?)
;(s/def :passport/hgt string?)
;(s/def :passport/hcl string?)
;(s/def :passport/ecl string?)
;(s/def :passport/pid string?)
;(s/def :passport/cid string?)

;; part 2

(defn check-in-range
  [at-least at-most v]
  (and
    (>= (parse-long v) at-least)
    (<= (parse-long v) at-most)))

(defn is-digit
  [s]
  (every? #(Character/isDigit %) s))

(s/def :passport/byr (s/and string?
                            #(= 4 (count %))
                            #(is-digit %)
                            #(check-in-range 1920 2002 %)))
(s/def :passport/iyr (s/and string?
                            #(= 4 (count %))
                            #(is-digit %)
                            #(check-in-range 2010 2020 %)))
(s/def :passport/eyr (s/and string?
                            #(= 4 (count %))
                            #(is-digit %)
                            #(check-in-range 2020 2030 %)))
(s/def :passport/hgt (s/or :cm (s/and string?
                                      #(str/includes? % "cm")
                                      #(is-digit (str/replace-first % #"cm" ""))
                                      #(check-in-range 150 193
                                                       (str/replace-first % #"cm" "")))
                           :in (s/and string?
                                      #(str/includes? % "in")
                                      #(is-digit (str/replace-first % #"in" ""))
                                      #(check-in-range 59 76
                                                       (str/replace-first % #"in" "")))))

(s/def :passport/hcl (s/and string?
                            #(str/includes? % "#")
                            #(= 7 (count %))
                            #(re-matches #"#[0-9|a-f]+" %)))
(s/def :passport/ecl (s/and string?
                            #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}))
(s/def :passport/pid (s/and string?
                            #(= 9 (count %))
                            #(is-digit %)))
(s/def :passport/cid string?)

(s/def :passport/passport (s/and
                            (s/map-of keyword? string?)
                            (s/keys :req [:passport/byr :passport/iyr :passport/eyr :passport/hgt
                                          :passport/hcl :passport/ecl :passport/pid]
                                    :opt [:passport/cid])))

(s/def :parser/line #(re-seq #"(\S+:\S+)+" %))
(s/def :parser/word #(re-find #"(\S+):(\S+)" %))

(defn parse-word
  "각 단어가 유효한 단어인지 확인한 후에 map 에 담는다."
  [word]
  {:pre [(s/valid? :parser/word word)]}
  (->> (re-find #"(\S+):(\S+)" word)
       rest
       (apply hash-map)))

(defn parse-line
  "pre condition 을 적용시켜 각 라인이 유효한지 확인한 후에 데이터를 map 형태로 만든다."
  [line]
  {:pre [(s/valid? :parser/line line)]}
  (->> (clojure.string/split line #" ")
       (map parse-word)
       (apply merge)
       (map (fn [[k v]]
              {(keyword "passport" k) v}))
       (into {})))

(defn parse-clause
  "하나의 여권정보를 담고있는 clause 를 파싱한다."
  [clause]
  (->> (map parse-line clause)
       (into {})))

(defn parse-input
  "empty line 을 기준으로 여권정보들이 나뉘어 있다.
  각 clause 들을 파싱하여 [{여권정보},...] 의 형태로 만든다."
  [source]
  (->> (map str (clojure.string/split-lines (slurp source)))
       (partition-by empty?)
       (filter #(not= [""] %))
       (map parse-clause)))

(comment
  (->> (parse-input "resources/2020_day4.txt")
       (filter #(s/valid? :passport/passport %))
       count)
  (->> (parse-input "resources/2020_day4_sample.txt")
       (filter #(s/valid? :passport/passport %))
       count))



