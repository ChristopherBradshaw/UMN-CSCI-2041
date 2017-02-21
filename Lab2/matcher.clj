(def nonempty?
  (fn [obj]
    (and
      (seq? obj)
      (not (empty? obj)))))

(def matching
  (fn [table pattern subject]
    (if
      (keyword? pattern)
      ; it's a keyword
      (let [currentPatternVal (get table pattern)]
        (if
          (= currentPatternVal nil)
          ; new keyword
          (assoc table pattern subject)
          ; old keyword
          (if
            (= currentPatternVal subject)
            ; matches old entry, OK
            table
            ; doesn't match, conflict 
            nil
          )
        )
      )
      ; not a keyword
      (if
        (or
          (nonempty? pattern)
          (nonempty? subject))
        ; at least one has something
        (if
          (and
            (nonempty? pattern)
            (nonempty? subject))
          ; they both have something
          (let [newTable (matching table (first pattern) (first subject))]
            (matching newTable (rest pattern) (rest subject))
          )
          ; only one has something
          nil
        )
        ; both are empty
        (if
          (= pattern subject)
          ; equal
          table
          ; not equal
          nil
        )
      )
    )
  )
)


(def match
  (fn [pattern subject]
    (matching (hash-map) pattern subject)
  )
)
