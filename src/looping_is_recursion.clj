(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc b e]
                   (if (zero? e) acc
                       (recur (* acc b) b (dec e))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [item s]
                 (if (empty? s) item
                   (recur (first s) (rest s))))]
        (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or (empty? seq1) (empty? seq2)) false
    (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [acc 0 p pred s a-seq]
    (cond
      (empty? s) nil
      (p (first s)) acc
      :else  (recur (inc acc) p (rest s)))))

(defn avg [a-seq]
  (loop [idx 1 sum (first a-seq) s a-seq]
    (let [r (rest s)]
      (cond (empty? a-seq) nil
          (empty? r) (/ sum idx)
          :else (recur (inc idx) (+ sum (first r)) r)))))

(defn parity [a-seq]
  (loop [res #{} s a-seq]
   (let [item (first s)]
     (cond
       (empty? s) res
       (contains? res item) (recur (disj res item) (rest s))
       :else (recur (conj res item) (rest s))))))


(defn fast-fibo [n]
  (loop [fibo (fn [cur next i]
               (if (zero? i) cur
                   (recur next (+ cur next) (dec i))))]
    (fibo 0N 1N n)))

(defn cut-at-repetition [a-seq]
  (loop [s a-seq uniq #{} res []]
    (if (empty? s) res
        (if (contains? uniq (first s)) res
            (recur (rest s) (conj uniq (first s)) (conj res (first s)))))))

