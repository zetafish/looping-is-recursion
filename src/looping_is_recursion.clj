
(ns looping-is-recursion)

(defn power [base exp]
  (loop [acc 1 n exp]
    (if (zero? n)
      acc
      (recur (* acc base) (dec n)))))

(defn last-element [a-seq]
  (cond (empty? a-seq) nil
        (empty? (rest a-seq)) (first a-seq)
        :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond (and (empty? seq1) (empty? seq2)) true
        (or (empty? seq1) (empty? seq2)) false
        (not= (first seq1) (first seq2)) false
        :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [n 0
         s a-seq]
    (cond (empty? s) nil
          (pred (first s)) n
          :else (recur (inc n) (rest s)))))

(defn avg [a-seq]
  (loop [sum 0
         count 0
         s a-seq]
    (if (empty? s)
      (/ sum count)
      (recur (+ (first s) sum) (inc count) (rest s)))))

(defn parity [a-seq]
  (loop [acc #{} s a-seq]
    (if (empty? s)
      acc
      (let [x (first s)]
        (recur ((if (contains? acc x) disj conj) acc x)
               (rest s))))))

(defn fast-fibo [n]
  (loop [f0 0 f1 1 n n]
    (cond (= 0 n) f0
          (= 1 n) f1
          :else (recur f1 (+ f0 f1) (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [set #{}
         acc []
         seq a-seq]
    (cond (empty? seq) acc
          (contains? set (first seq)) acc
          :else (recur (conj set (first seq))
                       (conj acc (first seq))
                       (rest seq)))))

