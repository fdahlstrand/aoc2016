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

(defn walk [instr state]
  (lazy-seq
   (if (empty? instr)
     [state]
     (cons state (walk (rest instr) (turn-and-move state (first instr)))))))

(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part 1
  (let [instructions (read-instructions "day01-puzzle-input.txt")
        [_ x y] (last (walk instructions [:north 0 0]))]
    (+ x y))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part 2
  )


