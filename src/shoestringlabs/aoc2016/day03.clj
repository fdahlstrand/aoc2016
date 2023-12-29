(ns shoestringlabs.aoc2016.day03
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn read-resource [path]
  (slurp (io/resource path)))

(defn parse-input [s]
  (map #(map parse-long (re-seq #"\d+" %)) (str/split-lines s)))

(defn valid-triangle?
  [[a b c]]
  (and
   (> (+ a b) c)
   (> (+ b c) a)
   (> (+ c a) b)))

(defn transpose [m] (apply mapv vector m))

(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part 1
  (->> (read-resource "day03-puzzle-input.txt")
       parse-input
       (filter valid-triangle?)
       count)
  ;; => 1032

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part 2
  (->> (read-resource "day03-puzzle-input.txt")
       parse-input
       (partition 3)
       (map transpose)
       (apply concat)
       (filter valid-triangle?)
       count)
  ;; => 1838
  )

