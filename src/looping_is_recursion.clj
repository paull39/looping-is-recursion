(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc hseq]
                 (if (empty? hseq)
                   acc
                   (recur (first hseq) (rest hseq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc hseq1 hseq2]
                 (cond
                   (= acc 0)
                     acc
                   (and (empty? hseq1) (empty? hseq2))
                     acc
                   (empty? hseq1)
                     0
                   (empty? hseq2)
                     0
                   ((complement =) (first hseq1) (first hseq2))
                     0
                  :else
                    (recur 1 (rest hseq1) (rest hseq2))))]
    (if (= 1 (helper 1 seq1 seq2))
      true
      false)))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         ha-seq a-seq]
    (cond
      (empty? ha-seq)
        nil
      (pred (first ha-seq))
        acc
      :else
        (recur (inc acc) (rest ha-seq)))))

(defn avg [a-seq]
  (if (empty? a-seq)
    nil
    (loop [acc 0
           helper-seq a-seq]
      (cond
        (empty? helper-seq)
         (/ acc (count a-seq))
        :else
          (recur (+ acc (first helper-seq)) (rest helper-seq))))))

(defn toggle [a-set elem]
  "if elem is in a-set
    add
    remove"
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  "Takes a seq and returns a set of those elements
  that occur an odd number of times in the sequence"
  (loop [accSet #{}
         helper-seq a-seq]
    (if (empty? helper-seq)
      accSet
      (recur (toggle accSet (first helper-seq)) (rest helper-seq)))))

(defn fast-fibo [n]
  (cond
    (= 0 n)
      0
    (< n 3)
      1
    :else
      (loop [acc 3
             accN1 1
             accN2 1]
        (if (= acc n)
          (+ accN1 accN2)
          (recur (inc acc) accN2 (+ accN2 accN1))))))

(defn cut-at-repetition [a-seq]
  "Takes in a sequence and returns elements from the sequence up to the first repetition."
  (loop [accSeq []
         helper-seq a-seq]
    (cond
      (empty? helper-seq)
        accSeq
      (< (count (set accSeq)) (count accSeq))
        (vec (butlast accSeq))
      :else
      (recur (conj accSeq (first helper-seq)) (rest helper-seq)))))

