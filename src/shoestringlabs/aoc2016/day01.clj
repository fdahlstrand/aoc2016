(ns shoestringlabs.aoc2016.day01
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn turn-right
  [[dir x y]]
  (case dir
    :north [:east  x y]
    :east  [:south x y]
    :south [:west  x y]
    :west  [:north x y]))

(defn turn-left
  [[dir x y]]
  (case dir
    :north [:west  x y]
    :west  [:south x y]
    :south [:east  x y]
    :east  [:north x y]))

(defn move
  [[dir x y] steps]
  (case dir
    :north [dir x (+ y steps)]
    :west  [dir (- x steps) y]
    :south [dir x (- y steps)]
    :east  [dir (+ x steps) y]))

(defn turn-and-move
  [state [turn steps]]
  (case turn
    :left  (move (turn-left  state) steps)
    :right (move (turn-right state) steps)))

(defn parse-move [s]
  (let [[_ turn steps] (re-find #"(L|R)(\d+)" s)]
    [(case (first turn) \L :left \R :right) (parse-long steps)]))

(defn parse-instructions [s]
  (map parse-move (str/split s #", ")))

(defn read-resource [path]
  (slurp (io/resource path)))

(def read-instructions (comp parse-instructions read-resource))

(defn make-path-segment
  [[_ sx sy] [_ ex ey]]
  (cond
    (< sx ex) (for [x (range sx ex  1)] [x sy])
    (> sx ex) (for [x (range sx ex -1)] [x sy])
    (< sy ey) (for [y (range sy ey  1)] [sx y])
    (> sy ey) (for [y (range sy ey -1)] [sx y])))

(defn path [instr [_ x y :as state]]
  (lazy-seq
   (if (empty? instr)
     [[x y]]
     (let [next-state (turn-and-move state (first instr))]
       (lazy-cat (make-path-segment state next-state)
                 (path (rest instr) next-state))))))

(defn distance [pos] (apply + (map #(Math/abs %) pos)))

(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part 1
  (->> [:north 0 0]
       (path (read-instructions "day01-puzzle-input.txt"))
       last
       distance)
  ;; => 301

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part 2
  (let [p       (path (read-instructions "day01-puzzle-input.txt") [:north 0 0])
        visited (into #{} (keep (fn [[x f]] (when (> f 1) x)) (frequencies p)))]
    (->> p
         (filter #(contains? visited %))
         (map-indexed (fn [ix pos] [ix pos]))
         (group-by (fn [[_ pos]] pos))
         (map (fn [[_ visit]] (second visit)))
         (sort-by (fn [[ix _]] ix))
         first
         ((fn [[_ pos]] (distance pos))))))
  ;; => 130
