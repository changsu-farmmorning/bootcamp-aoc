(ns util)

(defn parse-input
  ""
  [source]
  (map #(str %) (clojure.string/split-lines (slurp source))))