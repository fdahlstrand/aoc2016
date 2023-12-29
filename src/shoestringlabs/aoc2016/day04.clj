(ns shoestringlabs.aoc2016.day04
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn read-resource [path]
  (slurp (io/resource path)))

(defn parse-string [s]
  (let [[_ e id chk] (re-find #"([a-z-]+)(\d+)\[([a-z]{5})\]" s)]
    {:encrypted-name e
     :id             (parse-long id)
     :checksum       chk}))

(defn checksum
  [{s :encrypted-name}]
  (->> s
       (re-seq #"[a-z]+")
       str/join
       frequencies
       (sort-by (juxt (fn [kv] (- (val kv))) key))
       (map (fn [[k _]] k))
       (take 5)
       str/join))

(defn valid-checksum?
  [{expected :checksum :as entry}]
  (let [actual (checksum entry)]
    (= actual expected)))

(defn decode-char
  [enc ch]
  (char (+ (int \a)
           (mod (+ enc
                   (- (int ch) (int \a)))
                26))))

(defn decode-string [{enc :id s :encrypted-name}]
  (->> (str/split s #"-")
       (map #(map (partial decode-char enc) %))
       (map str/join)
       (str/join " ")))

(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part 1
  (->> (read-resource "day04-puzzle-input.txt")
       str/split-lines
       (map parse-string)
       (filter valid-checksum?)
       (map :id)
       (reduce +))
  ;; => 278221

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part 2
  (->> (read-resource "day04-puzzle-input.txt")
       str/split-lines
       (map parse-string)
       (filter valid-checksum?)
       (map (fn [e] [(:id e) (decode-string e)]))
       (filter (fn [[_ s]] (= s "northpole object storage")))
       first)
  ;; => [267 "northpole object storage"]
  )
