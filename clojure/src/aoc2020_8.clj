(ns aoc2020_8
  (:require [clojure.string :as str]))

(defn parse-line
  "문장 하나를 파싱하여 :type :val 로 변경하는 함수
  instruction 의 type 과 해당 값을 리스트로 가지고 있다."
  [line]
  (let [[inst val] (rest (re-find #"(\S+) (\S+)" line))]
    {:type (keyword inst) :val (parse-long val)}))

(defn parse-input
  "주어진 데이터를 파싱하여 백터 형태로 생성
  백터에는 각 명령어가 map 형태로 들어있다
  {:type :jmp :val 4}"
  [source]
  (->> (str/split-lines (slurp source))
       (map parse-line)
       (into [])))

(defn setup-program
  "프로그램의 초기 상태를 셋팅하는 함수. PC (program counter) 의 상태를 묘사한다고 생각하면 쉽다."
  [code]
  {:accumulator  0
   :inst-idx     0
   :inst-idx-set #{}
   :code         code})

(defn run-instruction
  "주어진 상황에서 수행해야하는 명령어를 수행하고 상태를 변경하는 작업
  :nop 는 다음 명령어로 이동
  :acc 는 :accumulator 를 val 만큼 증가시키고 다음 명령어로 이동
  :jmp 는 val 이후의 명령어로 이동
  명령어를 수행한 후 해당 명령어를 수행한 명령어 set 에 추가한다."
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
  "프로그램을 수행하는 함수
  마지막 명령어를 수행하거나 루프가 발견될 때까지 명령어를 하나씩 수행한다.
  결과적으로는 최종 상태만을 리턴"
  [program]
  (->> (iterate run-instruction program)
       (take-while (fn [{:keys [inst-idx inst-idx-set code]}]
                     (and
                       (nil? (inst-idx-set inst-idx))
                       (< inst-idx (count code)))))
       last))

(defn nth-idx-from-vector
  "nth v idx
  위와 같은 동작을 하는 함수. threading-last 를 위해 만듬"
  [idx v]
  (when (< idx (count v))
    (nth v idx)))

(defn find-nth-repairable-inst
  ":nop :jmp 명령어 중 n 번째 순서로 있는 명령어의 index 를 리턴
  어떤 명령어를 고쳐야 하는지 판단하기 위해 만듬"
  [code n]
  (->> (map-indexed vector code)
       (filter (fn [[_ {:keys [type]}]]
                 (or
                   (= type :nop)
                   (= type :jmp))))
       (nth-idx-from-vector (dec n))
       first))

(defn repair-nth-inst
  "n 번째 명령어를 수정
  :nop 인 경우는 :jmp 로, :jmp 의 경우는 :nop 로 수정"
  [program n]
  (let [code (:code program)
        target-idx (find-nth-repairable-inst code n)
        {:keys [type val]} (nth code target-idx)]
    (case type
      :nop (assoc-in program [:code target-idx] {:type :jmp :val val})
      :jmp (assoc-in program [:code target-idx] {:type :nop :val val}))))

(defn is-correct-run
  "작업이 정상 종료되었는지 확인
  정상 종료의 판단은 프로그램의 마지막 명령어가 수행되고 이후 더 수행할 명령어가 없는 경우이다."
  [{:keys [inst-idx code]}]
  (= (inc inst-idx) (count code)))

(defn nth-code-repair-run
  "n 번째 명령어를 수정하여 프로그램을 돌려본다."
  [program n]
  (->> (repair-nth-inst program n)
       run-program))

(defn find-nth-inst-to-correct-program
  "수정 가능한 명령어를 첫번째부터 수정하여 수정한 프로그램이 정상동작하는지 확인한다.
  정상 동작하면 몇 번째 명령어를 수정하였는지를 리턴하고
  그렇지 않다면 다음 명령어를 수정하여 다시 실행해본다."
  [program]
  (->> (iterate inc 1)
       (drop-while #(->> (nth-code-repair-run program %)
                         is-correct-run
                         not))
       first))

(defn run-correct-program
  "몇 번째 명령어를 수행할 때 정상동작하였는지를 확인하고,
  정상동작하는 프로그램을 수행시켜 마지막 상태를 리턴한다."
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

  (->> (parse-input "resources/2020_day8_sample.txt")
       setup-program
       run-program)
  (->> (parse-input "resources/2020_day8_sample.txt")
       setup-program
       run-correct-program
       :accumulator))