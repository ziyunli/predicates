(ns predicates)

(defn sum-f [f g x]
  (+ (f x) (g x)))

(defn less-than [n]
  (fn [k] (< k n)))

(defn equal-to [n]
  (fn [k] (== k n)))

(defn set->predicate [a-set]
  (fn [x] (contains? a-set x)))

(defn pred-and [pred1 pred2]
  (fn [x] (boolean (and (pred1 x) (pred2 x)))))

(defn pred-or [pred1 pred2]
  (fn [x] (boolean (or (pred1 x) (pred2 x)))))

(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
  (every? whitespace? string))

(defn has-award? [book award]
  (contains? (:awards book) award))

(defn HAS-ALL-THE-AWARDS? [book awards]
  (every? (set->predicate (:awards book)) awards))

(defn my-some [pred a-seq]
  (let [filtered (filter pred a-seq)
        is-hit? (> (count filtered) 0)]
    (if is-hit? (pred (first filtered)) nil)))

(defn my-every? [pred a-seq]
  (let [filtered (filter pred a-seq)
        filtered-count (count filtered)]
    (= filtered-count (count a-seq))))

(defn prime? [n]
  (let [pred (fn [k] (= (mod n k) 0))]
    (not (some pred (range 2 n)))))
;^^
