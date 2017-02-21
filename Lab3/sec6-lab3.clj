(declare merging)
(declare splitting)
(declare mergesort)

(def prepend-to-seq
  (fn [elm con]
    (concat (conj '() elm) con)
  )
)

(def merging 
  (fn [less? left right]
    (if
      (empty? left)
      ; left is empty
      right
      ; else
      (if 
        (empty? right)
        ; right is empty
        left
        ; exists a value in both left and right
        (let [leftFirst (first left)
              rightFirst (first right)]
          (if
            (less? leftFirst rightFirst)
            ; left < right
            (prepend-to-seq leftFirst (merging less? (rest left) right))
            ; left >= right
            (prepend-to-seq rightFirst (merging less? left (rest right)))
          )
        )
      )
    )
  )
)

(def splitting 
  (fn [less? unsorted left right]
    (if
      (empty? unsorted)
      ; seq is empty
      (let [leftSorted (mergesort less? left)
            rightSorted (mergesort less? right)]
        (merging less? leftSorted rightSorted)
      )
      ; else
      (if
        (empty? (rest unsorted))
        ; there's 1 element in the seq
        (let [leftSorted (mergesort less? (prepend-to-seq (first unsorted) left))
              rightSorted (mergesort less? right)]
          (merging less? leftSorted rightSorted)
        )
        ; 2 or more elements in the seq
        (let [e1 (first unsorted)
              unsorted-rest (rest unsorted)
              e2 (first unsorted-rest)
              unsorted-rest (rest unsorted-rest)]
          (splitting less? unsorted-rest (prepend-to-seq e1 left) (prepend-to-seq e2 right))
        )
      )
    )
  )
)



; wrapper
(def mergesort
  (fn [less? unsorted]
    (if
      (or
        (empty? unsorted)
        (empty? (rest unsorted))
      )
      ; less than 2 elements in seq
      unsorted
      ; else
      (splitting less? unsorted '() '())
    )
  )
)
