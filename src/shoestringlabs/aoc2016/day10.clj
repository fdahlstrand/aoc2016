(ns shoestringlabs.aoc2016.day10
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn is-bot-id? [id] (str/starts-with? (name id) "bot"))

(defn bot [id] (keyword (str/join ["bot-" (str id)])))
(defn output [id] (keyword (str/join ["output-" (str id)])))
(defn id [t id]
  (if (str/starts-with? t "bot")
    (bot id)
    (output id)))

(defn give
  [id v factory]
  (let [bot (get-in factory [:bots id])
        output (get-in factory [:outputs id])]
    (if (is-bot-id? id)
      (if (:left bot)
        (assoc-in factory [:bots id :right] v)
        (assoc-in factory [:bots id :left] v))
      (assoc-in factory [:outputs id] (cons v output)))))

(defn empty-hands
  [bot-id factory]
  (-> factory
      (assoc-in [:bots bot-id :left] nil)
      (assoc-in [:bots bot-id :right] nil)))

(defn process
  [bot-id a b factory]
  (let [p (get-in factory [:process bot-id])]
    (assoc-in factory [:process bot-id] (cons #{a b} p))))

(defn bot-do
  [bot-id low-id high-id factory]
  (let [left (get-in factory [:bots bot-id :left])
        right (get-in factory [:bots bot-id :right])]
    (if (< left right)
      (->> factory
           (process bot-id left right)
           (give low-id left)
           (give high-id right)
           (empty-hands bot-id))
      (->> factory
           (process bot-id left right)
           (give low-id right)
           (give high-id left)
           (empty-hands bot-id)))))

(defn read-resource [path]
  (slurp (io/resource path)))

(defn parse-instruction
  [s]
  (case (first s)
    \v (let [[_ v bot-id]
             (re-find #"value (\d+) goes to bot (\d+)" s)]
         {:type :assign :value (parse-long v) :bot bot-id})
    \b (let [[_ bot-id ltgt lid htgt hid]
             (re-find #"bot (\d+) gives low to (output|bot) (\d+) and high to (output|bot) (\d+)" s)]
         {:type :give :bot-id bot-id :low-target ltgt :low-id lid :high-target htgt :high-id hid})))

(defn read-instructions
  [s]
  (->> s
       str/split-lines
       (map parse-instruction)
       (group-by :type)))

(defn make-bot
  [{bot-id :bot-id ltgt :low-target lid :low-id htgt :high-target hid :high-id}]
  {:bot-id (bot bot-id)
   :left nil
   :right nil
   :instr (partial bot-do (bot bot-id) (id ltgt lid) (id htgt hid))})

(defn make-bots
  [instrs]
  (map make-bot instrs))

(defn init-factory
  [instrs factory]
  (reduce (fn [f {bot-id :bot v :value}] (give (bot bot-id) v f)) factory instrs))

(defn hands-full? [[_ {l :left r :right}]] (and l r))

(defn run-factory
  [s]
  (let [instrs (read-instructions s)
        new-factory (init-factory (:assign instrs) {:bots (into {} (map (fn [{id :bot-id :as bot}] {id bot}) (make-bots (:give instrs)))) :outputs {}})]
    (loop [[_ b] (first (filter hands-full? (:bots new-factory)))
           factory new-factory]
      (if (nil? b)
        factory
        (let [next-factory ((:instr b) factory)]
          (recur (first (filter hands-full? (:bots next-factory))) next-factory))))))

(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part 1
  (->> (read-resource "day10-puzzle-input.txt")
       run-factory
       :process
       (remove (fn [[_ processed]] (empty? (filter #(= % #{61 17}) processed))))
       first)
  ;; => [:bot-56 (#{61 17})]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part 2
  (->> (read-resource "day10-puzzle-input.txt")
       run-factory
       :outputs
       ((juxt :output-0 :output-1 :output-2))
       (map first)
       (reduce *))
  ;; => 7847
  )

