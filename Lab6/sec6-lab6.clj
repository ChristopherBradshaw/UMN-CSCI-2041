(def unique
  (fn [F S]
    (if
      ; if we have more elements to work on
      (and (seq? S) (not (empty? S)))
      (do
        (if
          ; if this is the last occurence of an element, run the function
          (not (some (fn [n] (= n (first S))) (rest S)))
          (F (first S))
        )
        (recur F (rest S))
      )
    )
  )
)

; (map-elm F a '(x y z)) => (F(a,x), F(a,y), F(a,z))
(def map-elm
  (fn [F elm S]
    (if
      (and (seq? S) (not (empty? S)))
      (concat (list (F elm (first S))) (map-elm F elm (rest S)))
      '()
    )
  )
)

(def cartesian
  (fn [F u v]
    (if
      (and (seq? u) (not (empty? u)))
      (concat (map-elm F (first u) v) (cartesian F (rest u) v))
      '()
    )
  )
)

(def nest
  (fn [F N]
    (case N
      0 (fn [n] n)
      1 F
      (comp F (nest F (- N 1)))
    )
  )
)
