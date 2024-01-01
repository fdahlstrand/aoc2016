(ns shoestringlabs.aoc2016.day08
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn make-screen
  [width height]
  (into [] (for [_ (range 0 height)]
             (into [] (for [_ (range 0 width)]
                        \.)))))

(defn width [screen] (count (get screen 0)))
(defn height [screen] (count screen))

(defn rotate-screen
  [screen]
  (apply mapv vector screen))

(defn get-pixel
  [row col screen]
  (get (get screen row) col))

(defn set-pixel
  ([screen [row col]] (set-pixel screen row col))
  ([screen row col] (assoc screen row (assoc (get screen row) col \#))))

(defn set-pixels
  [coll screen]
  (reduce set-pixel screen coll))

(defn print-screen
  [screen]
  (println
   (str/join "\n"
             (for [row (range 0 (height screen))]
               (str/join
                (for [col (range 0 (width screen))]
                  (get-pixel row col screen)))))))

(defn rect
  [width height screen]
  (let [pixels (for [row (range 0 height)
                     col (range 0 width)]
                 [row col])]
    (set-pixels pixels screen)))

(defn rotate
  [line step]
  (let [len (count line)]
    (into [] (for [x (range 0 (count line))]
               (get line (mod (+ len (- x step)) len))))))

(defn rotate-row
  [row step screen]
  (assoc screen row (rotate (get screen row) step)))

(defn rotate-column
  [col step screen]
  (rotate-screen (rotate-row col step (rotate-screen screen))))

(defn parse-rect
  [s]
  (let [[_ w h] (re-find #"rect (\d+)x(\d+)" s)]
    (partial rect (parse-long w) (parse-long h))))

(defn parse-rotate-row
  [s]
  (let [[_ row step] (re-find #"rotate row y=(\d+) by (\d+)" s)]
    (partial rotate-row (parse-long row) (parse-long step))))

(defn parse-rotate-column
  [s]
  (let [[_ col step] (re-find #"rotate column x=(\d+) by (\d+)" s)]
    (partial rotate-column (parse-long col) (parse-long step))))

(defn parse-op
  [s]
  (cond
    (str/starts-with? s "rect ") (parse-rect s)
    (str/starts-with? s "rotate row y=") (parse-rotate-row s)
    (str/starts-with? s "rotate column x=") (parse-rotate-column s)))

(defn read-resource [path]
  (slurp (io/resource path)))

(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part 1
  (->> (read-resource "day08-puzzle-input.txt")
       str/split-lines
       (map parse-op)
       (reduce #(%2 %1) (make-screen 50 6))
       flatten
       (filter #(= % \#))
       count)
  ;; => 115

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part 2
  (->> (read-resource "day08-puzzle-input.txt")
       str/split-lines
       (map parse-op)
       (reduce #(%2 %1) (make-screen 50 6))
       print-screen)
  ;; => ####.####.####.#...##..#.####.###..####..###...##.
  ;; => #....#....#....#...##.#..#....#..#.#......#.....#.
  ;; => ###..###..###...#.#.##...###..#..#.###....#.....#.
  ;; => #....#....#......#..#.#..#....###..#......#.....#.
  ;; => #....#....#......#..#.#..#....#.#..#......#..#..#.
  ;; => ####.#....####...#..#..#.#....#..#.#.....###..##..
  )
