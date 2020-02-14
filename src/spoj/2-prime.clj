(require '[clojure.string :as str])

(defn parse-int [s] (java.lang.Integer/parseInt s))

(defn parse-case [s]
  (map parse-int (str/split s #" ")))

(defn read-cases
  "Reads cases according to the task and returns seq of [start end] vectors"
  []
  (let [n (parse-int (read-line))]
    (loop [x n cases []]
      (if-not (= x 0)
        (recur (dec x) (conj cases (parse-case (read-line))))
        cases))))

(defn is-prime [x m]
  (if (= (mod x 2) 0)
    false
    (not (some (fn [[k _]] (= (mod x k) 0)) m))))

(defn is-prime-sequential [x acc last-checked]
  (if (> x last-checked)
    (let [x-is-prime (is-prime x acc)]
      (when (= x-is-prime true) (set! prime-numbers (assoc prime-numbers x true)))
      (set! last-checked x)
      x-is-prime)
    (contains? prime-numbers x)))

(defn gen-prime-numbers [from to all-primes]
  (loop [i from primes all-primes]
    (if (> i to)
      primes
      (if (is-prime i primes)
        (recur (inc i) (assoc primes i true))
        (recur (inc i) primes)))))

(defn fill-prime-map [x acc]
  (set! last-checked x)
  (gen-prime-numbers (inc last-checked) x acc))

(defn ensure-prime-generated [x acc last-checked]
  (if (> x last-checked)
    (fill-prime-map x acc)
    acc))

;(defn gen-prime-numbers [from to]
;  (loop [i from primes {}]
;    (if (> i to)
;      primes
;      (if (is-prime i primes)
;        (recur (inc i) (assoc primes i true))
;        (recur (inc i) primes)))))

(defn print-if-prime [i acc last-checked]
  (let [[is-prime acc last-checked] (is-prime-sequential i acc last-checked)]
    (when is-prime (println i))
    [acc last-checked])


  (when (is-prime-sequential i acc last-checked) (println i)))

(defn print-primes
  "Accepts start and end of range, accumulated map of existing prime numbers in shape of {:number true}
  prints prime numbers in range and returns extended(or the same) map"
  [start end acc last-checked]
  (let [[acc last-checked] (ensure-prime-generated start acc last-checked)]
    (reduce (fn [[last-checked acc] i] (print-if-prime i acc last-checked)) [[last-checked acc] start] (range (dec start) (inc end)))
    (println)))

(defn process-case [[last-checked acc] [a b]] (print-primes a b acc last-checked))

(reduce process-case [0 {}] (read-cases))



