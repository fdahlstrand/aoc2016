(ns shoestringlabs.aoc2016.day02
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn read-resource [path]
  (slurp (io/resource path)))

(defn parse-line [s]
  (map #(case %
          \R :right
          \U :up
          \D :down
          \L :left) s))

(defn parse-instructions [s]
  (map parse-line (str/split-lines s)))

(defn get-key
  ([pad row col] (get (get pad row) col))
  ([pad [row col]] (get-key pad row col)))

(defn move
  [offset pad pos]
  (let [newpos (map + pos offset)]
    (if (get-key pad newpos) newpos pos)))

(def move-up    (partial move [-1  0]))
(def move-down  (partial move [1  0]))
(def move-left  (partial move [0 -1]))
(def move-right (partial move [0  1]))

(defn follow-instr
  [pad pos instr]
  (case instr
    :left  (move-left pad pos)
    :right (move-right pad pos)
    :down  (move-down pad pos)
    :up    (move-up pad pos)))

(defn follow-line
  [pad pos instrs]
  (lazy-seq
   (if (empty? instrs)
     [pos]
     (cons pos
           (follow-line pad
                        (follow-instr pad pos (first instrs))
                        (rest instrs))))))

(defn follow-lines
  [pad pos lines]
  (lazy-seq
   (if (empty? lines)
     '()
     (let [line (follow-line pad pos (first lines))
           next-pos (last line)]
       (cons line
             (follow-lines pad next-pos (rest lines)))))))

(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  ;; Part 1
  (let [keypad [[\1 \2 \3]
                [\4 \5 \6]
                [\7 \8 \9]]]
    (->> (parse-instructions (read-resource "day02-puzzle-input.txt"))
         (follow-lines keypad '(1 1))
         (map last)
         (map #(get-key keypad %))
         (str/join)))
  ;; => "18843"

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
  ;; Part 2
  (let [keypad [[nil nil  \1 nil nil]
                [nil  \2  \3  \4 nil]
                [\5   \6  \7  \8  \9]
                [nil  \A  \B  \C nil]
                [nil nil  \D nil nil]]]
    (->> (parse-instructions (read-resource "day02-puzzle-input.txt"))
         (follow-lines keypad '(2 0))
         (map last)
         (map #(get-key keypad %))
         (str/join))))
   ;; => "67BB9"
