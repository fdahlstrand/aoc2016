(ns shoestringlabs.aoc2016.day07
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn read-resource [path]
  (slurp (io/resource path)))

(defn elem->seg
  [e]
  (if (str/starts-with? e "[") ;; ]
    {:hyper (str/replace e #"\[|\]" "")}
    {:seg e}))

(defn ABBA?
  [[a b c d]]
  (and (= a d) (= b c) (not= a b)))

(defn ABA?
  [[a b c]]
  (and (= a c) (not= a b)))

(defn ABA->BAB
  [[a b _]]
  (list b a b))

(defn contains-ABBA?
  [s]
  (->> (partition 4 1 s)
       (map ABBA?)
       (some true?)))

(defn falsey? [x] (or (nil? x) (false? x)))

(defn collect-ABAs
  [s]
  (->> s
       (map #(partition 3 1 %))
       (apply concat)
       (filter ABA?)))

(defn support-TLS?
  [ip]
  (let [segs (keep :seg ip)
        hypers (keep :hyper ip)]
    (and (some true? (map contains-ABBA? segs))
         (every? falsey? (map contains-ABBA? hypers)))))

(defn support-SSL?
  [ip]
  (let [ABAs (->> ip
                  (keep :seg)
                  collect-ABAs
                  (map ABA->BAB)
                  (map str/join))
        BABs (->> ip
                  (keep :hyper)
                  collect-ABAs
                  (map str/join)
                  (into #{}))]
    (some true? (map #(contains? BABs %) ABAs))))

(defn parse-IP
  [s]
  (->> (re-seq #"\[[a-z]+\]|[a-z]+" s)
       (map elem->seg)))

(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part 1
  (->> (read-resource "day07-puzzle-input.txt")
       str/split-lines
       (map parse-IP)
       (filter support-TLS?)
       count)
  ;; => 118

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part 2
  (->> (read-resource "day07-puzzle-input.txt")
       str/split-lines
       (map parse-IP)
       (filter support-SSL?)
       count)
  ;; => 260
  )
