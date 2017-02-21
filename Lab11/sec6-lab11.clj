(def partial-ordering
  (hash-map
    'A #{'C}
    'B #{'H}
    'C #{'G}
    'G #{'D 'E}
    'D #{'F}
    'E #{'H}
    'H #{'F}
    'I #{'B 'E}))

(defn satisfy 
  ([precedes? objects] (satisfy precedes? '() objects))
  ([precedes? ret-seq objects]
    (if (or (= nil objects) (empty? objects))
      ret-seq
      (unpreceded
        (fn [x]
          (if (= nil x)
            nil
            (satisfy precedes? (concat ret-seq (list x)) (delete objects x))
          )
        )
        precedes? objects
      )
    )
  )
)

(defn unpreceded 
  ([etc precedes? objects] (unpreceded etc precedes? 0 objects))
  ([etc precedes? index objects]
    (if (or (= nil objects) (>= index (count objects)))
      nil
      (if (unpreceded? precedes? (nth objects index) objects)
        (etc (nth objects index))
        (recur etc precedes? (+ index 1) objects)
      )
    )
  )
)

; Return true if object is not preceded by any object in other-objects
(defn unpreceded? [precedes? object other-objects]
  (not (some #(precedes? % object) other-objects))
)

(defn precedes? [left right]
  (contains? (get partial-ordering left) right)
)

(defn delete [objects object]
  (remove (partial = object) objects)
)
