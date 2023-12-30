(ns shoestringlabs.aoc2016.day06
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn read-resource [path]
  (slurp (io/resource path)))

(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part 1
  (->> (read-resource "day06-puzzle-input.txt")
       str/split-lines
       (map #(into [] %))
       (apply mapv vector)
       (map frequencies)
       (map #(apply max-key val %))
       (map key)
       str/join)
  ;; => "agmwzecr"

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part 2
  (->> (read-resource "day06-puzzle-input.txt")
       str/split-lines
       (map #(into [] %))
       (apply mapv vector)
       (map frequencies)
       (map #(apply min-key val %))
       (map key)
       str/join)
  ;; => "owlaxqvq"
  )
