(declare lambda?)
(declare lambda-call?)
(declare lambda-fn?)
(declare lambda-parameters?)
(declare lambda-symbol?)

(defn in? [elm coll]
  (some #(= elm %) coll)
)

(defn lambda? 
  ([L] (lambda? L '()))
  ([L S]
    (cond
      (nil? L) nil
      (symbol? L) (lambda-symbol? L S)
      (= (first L) 'fn) (lambda-fn? L S)
      :default (lambda-call? L S)
    )
  )
)

(defn lambda-call? [L S]
  (if (or (= nil L) (empty? L))
    true
    (and (lambda? (first L) S) (if (not (= nil (rest L))) (lambda-call? (rest L) S) true))
  )
)

(defn lambda-fn? [L S]
  (if (lambda-parameters? (second L))
    (lambda? (first (rest (rest L))) (cons (second L) S))
    false
  )
)

(defn lambda-parameters? [P]
  (and (vector? P) (= (count P) (count (distinct P))))
)

(defn lambda-symbol? [N S]
  (if (or (empty? S) (= S nil)) 
    false
    (if (in? N (first S))
      true
      (lambda-symbol? N (rest S))
    )
  )
)
