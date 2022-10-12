(ns aoc2020_8
  (:require [clojure.string :as str]))

(defn parse-line
  ""
  [line]
  (let [[inst val] (rest (re-find #"(\S+) (\S+)" line))]
    {:type (keyword inst) :val (parse-long val)}))

(defn parse-input
  ""
  [source]
  (->> (str/split-lines (slurp source))
       (map parse-line)
       (into [])))

(defn setup-program
  ""
  [code]
  {:accumulator  0
   :inst-idx     0
   :inst-idx-set #{}
   :code         code})

(defn run-instruction
  ""
  [{:keys [inst-idx code] :as program}]
  (let [{:keys [type val]} (nth code inst-idx)]
    (case type
      :nop (-> program
               (update :inst-idx inc)
               (update :inst-idx-set conj inst-idx))
      :acc (-> program
               (update :inst-idx inc)
               (update :accumulator + val)
               (update :inst-idx-set conj inst-idx))
      :jmp (-> program
               (update :inst-idx + val)
               (update :inst-idx-set conj inst-idx)))))

(defn run-program
  ""
  [program]
  (->> (iterate run-instruction program)
       (take-while (fn [{:keys [inst-idx inst-idx-set code]}]
                     (and
                       (nil? (inst-idx-set inst-idx))
                       (< inst-idx (count code)))))
       last))

(defn nth-idx-from-vector
  ""
  [idx v]
  (if (< idx (count v))
    (nth v idx)))

(defn find-nth-repairable-inst
  ""
  [code n]
  (->> (map-indexed vector code)
       (filter (fn [[_ {:keys [type]}]]
                 (or
                   (= type :nop)
                   (= type :jmp))))
       (nth-idx-from-vector (dec n))
       first))

(defn repair-nth-inst
  ""
  [program n]
  (let [code (:code program)
        target-idx (find-nth-repairable-inst code n)
        {:keys [type val]} (nth code target-idx)]
    (case type
      :nop (assoc program :code
                          (assoc code target-idx {:type :jmp :val val})) ;assoc-in
      ;assoc-in
      :jmp (assoc program :code
                          (assoc code target-idx {:type :nop :val val})))))

(defn is-correct-run
  ""
  [{:keys [inst-idx code]}]
  (= (inc inst-idx) (count code)))

(defn nth-code-repair-run
  ""
  [program n]
  (->> (repair-nth-inst program n)
       run-program))

;(defn find-correct-program
;  ""
;  [program]
;  (->> (iterate inc 1)
;       (map #(nth-code-repair-run % program))
;       (take-while #(not (is-correctly-run %)))))

; [false false false true false false ...]

; drop-while

(defn find-nth-inst-to-correct-program
  ""
  [program]
  (->> (iterate inc 1)
       (take-while #(->> (nth-code-repair-run program %)
                         is-correct-run
                         not))
       last
       inc))

(defn run-correct-program
  ""
  [program]
  (->> (find-nth-inst-to-correct-program program)
       (nth-code-repair-run program)))


(comment
  (->> (parse-input "resources/2020_day8.txt")
       setup-program
       run-program
       :accumulator)
  (->> (parse-input "resources/2020_day8.txt")
       setup-program
       run-correct-program
       :accumulator)

  (#{1 2 3} 5)

  (->> (parse-input "resources/2020_day8_sample.txt")
       setup-program
       run-program)
  (->> (parse-input "resources/2020_day8_sample.txt")
       setup-program
       run-correct-program
       :accumulator))