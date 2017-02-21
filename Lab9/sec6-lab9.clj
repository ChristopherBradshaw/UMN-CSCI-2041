(declare euler)
(declare next-term)
(declare remaining-termins)
(declare sum)

(defn euler 
  ([x] (euler x 1 0 0))
  ; x - numerator constant
  ; current numerator
  ; current denomenator
  ; current term
  ([x n d c]
    (if (= d 0)
      (list (/ n 1) (fn [] (euler x (* x n) 1 1)))
      (list (/ n d) (fn [] (euler x (* x n) (* d (+ c 1)) (+ c 1))))
    )
  )
)

(defn next-term [terms]
  (first terms)
)

(defn remaining-terms [terms]
  ((first (rest terms)))
)

(defn sum [terms epsilon]
  (let [t (next-term terms)]
    (if (< t epsilon)
      0.0
      (+ t (sum (remaining-terms terms) epsilon))
    )
  )  
)
