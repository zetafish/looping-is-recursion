
(ns looping-is-recursion)

(defn power [base exp]
  (let [aux (fn [acc n]
              (if (zero? n)
                acc
                (recur (* acc base) (dec n))))]
    (aux 1 exp)))

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
  (let [aux (fn [n s]
              (cond (empty? s) nil
                    (pred (first s)) n
                    :else (recur (inc n) (rest s))))]
    (aux 0 a-seq)))

(defn avg [a-seq]
  (let [aux (fn [sum count s]
              (if (empty? s)
                (/ sum count)
                (recur (+ (first s) s) (inc count) (rest s))))]
    (aux 0 0 a-seq)))

(defn parity [a-seq]
  (let [aux (fn [acc s]
              (if (empty? s)
                acc
                (let [x (first s)]
                  (recur ((if (contains? acc x) disj conj) acc x)
                         (rest s)))))]
    (aux #{} a-seq)))

(defn fast-fibo [n]
  (let [aux (fn [f0 f1 n]
              (cond (= 0 n) f0
                    (= 1 n) f1
                    :else (recur f1 (+ f0 f1) (dec n))))]
    (aux 0 1 n)))

(defn cut-at-repetition [a-seq]
  (loop [set #{}
         acc []
         seq a-seq]
    (cond (empty? seq) acc
          (contains? set (first seq)) acc
          :else (recur (conj set (first seq))
                       (conj acc (first seq))
                       (rest seq)))))

