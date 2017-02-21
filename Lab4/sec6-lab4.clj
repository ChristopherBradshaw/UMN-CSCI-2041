(def operatorMap
  (hash-map
    '+ '(add)
    '* '(mul)
    '- '(sub)
    '/ '(div)
  )
)

(def operator
  (fn [form]
    (conj '() (get operatorMap (first form)))
  )
)

(def left-arg
  (fn [form]
    (first (rest form))
  )
)

(def right-arg
  (fn [form]
    (first (rest (rest form)))
  )
)

(def _stackify
  (fn [form]
    (if
      (seq? form)
      (if
        (and 
          (= (first form) '-)
          (= (first (rest (rest form))) nil)
        )
        (concat (_stackify (left-arg form)) '((neg)))
        (concat (_stackify (left-arg form)) (_stackify (right-arg form)) (operator form))
      )
      (cons (concat '(push) (conj '() form)) '())
    )
  )
)

(def stackify
  (fn [form]
    (concat (_stackify form) (cons '(pop) '()))
  )
)
