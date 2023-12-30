(ns shoestringlabs.aoc2016.day05
  (:require
   [clojure.string :as str]))

(import java.security.MessageDigest
        java.math.BigInteger)

(defn md5
  [^String s] ;; Type hinting to avoid reflection
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw       (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn starts-with-00000? [s] (str/starts-with? s "00000"))

(defn test-seq
  [seed n]
  (lazy-seq
   (cons (str/join [seed (str n)]) (test-seq seed (inc n)))))

(defn hex->int
  [ch]
  (case ch
    (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9) (- (int ch) (int \0))
    (\a \b \c \d \e \f)             (+ 10 (- (int ch) (int \a)))))

(defn decrypt
  [code [x & xs]]
  (if (= (count code) 8)
    code
    (if-not (starts-with-00000? x)
      (recur xs code)
      (let [slot (hex->int (nth x 5))
            ch   (nth x 6)]
        (cond
          (contains? code slot) (recur xs code)
          (<= 0 slot 7)         (recur xs (assoc code slot ch))
          :else                 (recur xs code))))))

(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part 1
  (->> (test-seq "ffykfhsq" 0)
       (map md5)
       (filter starts-with-00000?)
       (take 8)
       (map #(nth % 5))
       str/join)
  ;; => "c6697b55"

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part 2
  (str/join
   (for [slot '(\0 \1 \2 \3 \4 \5 \6 \7)]
     (->> (test-seq "ffykfhsq" 0)
          (map md5)
          (filter #(str/starts-with? % (str/join ["00000" slot])))
          first
          (#(nth % 6)))))
  ;; => "8c35d1ab"
  )


