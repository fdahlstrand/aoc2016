(ns shoestringlabs.aoc2016.day09
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn read-resource [path]
  (slurp (io/resource path)))

(defn consume-token [s t] (subs s (count (:raw t))))

(defn parse-text
  [s]
  (let [raw (re-find #"^[^\(]+" s) ;)
        token {:type :text
               :raw  raw}]
    [token (consume-token s token)]))

(defn parse-marker
  [s]
  (let [[raw l r] (re-find #"\((\d+)x(\d+)\)" s)
        len       (parse-long l)
        token {:type   :marker
               :raw    (subs s 0 (+ len (count raw)))
               :marker raw
               :text   (subs s (count raw) (+ len (count raw)))
               :rep    (parse-long r)}]
    [token (consume-token s token)]))

(defn tokenize
  [s]
  (lazy-seq
   (let [ch (first s)]
     (case ch
       nil '()
       \( (let [[token rs] (parse-marker s)] ;)
            (cons token (tokenize rs)))
       (let [[token rs] (parse-text s)]
         (cons token (tokenize rs)))))))

(defn size-token
  [t]
  (case (:type t)
    :text   (count (:raw t))
    :marker (* (:rep t) (count (:text t)))))

(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part 1
  (->> (read-resource "day09-puzzle-input.txt")
       str/trim
       tokenize
       (map size-token)
       (reduce +))
  ;; => 70186
  )


