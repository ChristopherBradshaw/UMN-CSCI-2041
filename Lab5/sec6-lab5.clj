(require '[clojure.set :as set])

(def thru
  (fn [etc start end]
    (loop
      [index start]
      (if
        (< index end)
        (do
          (etc index)
          (recur (+ index 1))
        )
      )
    )
  )
)

(def choosing
  (fn [etc c n k e]
    (if
      (= k 0)
      (etc c)
      (thru
        ; callback
        (fn [i]
          (choosing etc (set/union c (conj #{} i)) n (- k 1) (+ i 1))
        )
        e ; from
        n ; to
      )
    )
  )
)

(def choose
  (fn [etc n k]
    (choosing etc #{} n k 0)
  )
)
