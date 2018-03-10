(ns alphabet-cipher.coder)
(require '[clojure.string :as str])

(let
 [a-int (int \a)]
  (defn int-to-char [n] (char (+ a-int (mod (+ n 26) 26))))
  (defn char-to-int [ch] (- (int ch) a-int)))

(defn encode [keyword message]
  (str/join
   (map #(int-to-char (+ %2 %1))
        (map char-to-int (cycle keyword))
        (map char-to-int message))))

(defn decode [keyword message]
  (str/join
   (map #(int-to-char (- %2 %1))
        (map char-to-int (cycle keyword))
        (map char-to-int message))))

(defn find-repeats [keyword]
  (loop [i 2]
    (let [parts (partition i keyword)]
      (cond
        (empty? parts) nil
        (apply = parts) (first parts)
        :else (recur (inc i))))))

(defn decipher [cipher message]
  ((comp str/join find-repeats)
   (map #(int-to-char (- %1 %2))
        (map char-to-int cipher)
        (map char-to-int message))))
