; Forward declarations

(declare ifify)
(declare normalize)
(declare simplify)
(declare substitute)
(declare tautology?)

; Utilities

(defn op [form] (first form))
(defn left [form] (second form))
(defn right [form] (first (rest (rest form))))

(defn if? [form] (and (seq? form) (= 'if (first form)) (or (let [nForm (count form)] (= nForm 3) (= nForm 4)))))
(defn not? [form] (and (seq? form) (= 'not (op form)) (= (count form) 2)))
(defn and? [form] (and (seq? form) (= 'and (op form)) (= (count form) 3)))
(defn or? [form] (and (seq? form) (= 'or (op form)) (= (count form) 3)))
(defn imply? [form] (and (seq? form) (= 'imply (op form)) (= (count form) 3)))
(defn equiv? [form] (and (seq? form) (= 'equiv (op form)) (= (count form) 3)))

(defn if-test [form] (assert (if? form)) (first (rest form)))
(defn if-then [form] (assert (if? form)) (first (rest (rest form))))
(defn if-else [form] (assert (if? form)) (first (rest (rest (rest form)))))

; Rules

; Rule 1 - (not a) => (if a false true)
(defn build-not [form]
  (assert (not? form))
  `(if ~(ifify (left form)) false true)
)

; Rule 2 - (and a b) => (if a b false)
(defn build-and [form]
  (assert (and? form))
  `(if ~(ifify (left form)) ~(ifify (right form)) false)
)

; Rule 3 - (or a b) => (if a true b)
(defn build-or [form]
  (assert (or? form))
  `(if ~(ifify (left form)) true ~(ifify (right form)))
)

; Rule 4 - (imply a b) => (if a b true)
(defn build-imply [form]
  (assert (imply? form))
  `(if ~(ifify (left form)) ~(ifify (right form)) true)
)

; Rule 5 - (equiv a b) => (if a b (if b false true))
(defn build-equiv [form]
  (assert (equiv? form))
  `(if ~(ifify (left form)) ~(ifify (right form)) (if ~(ifify (right form)) false true))
)

; Rule 6 - (if (if x a1 b1) a2 b2) => (if x (if a1 a2 b2) (if b1 a2 b2))
(defn normalize-if [form]
  ; If this if statement's condition is another if statement,
  ; we need to simplify further
  (if (if? form)
    (if (if? (if-test form))
      (let [x (normalize (if-test (if-test form)))
            a1 (normalize (if-then (if-test form))) b1 (normalize (if-else (if-test form)))
            a2 (normalize (if-then form)) b2 (normalize (if-else form))]
        (normalize `(if ~x ~(normalize `(if ~a1 ~a2 ~b2)) ~(normalize `(if ~b1 ~a2 ~b2))))
      )
      (if (and (if? (if-then form)) (if? (if-test (if-then form))))
        (normalize `(if ~(if-test form) ~(normalize (if-then form)) ~(if-else form)))
        (if (and (if? (if-else form)) (if? (if-test (if-else form))))
          (normalize `(if ~(if-test form) ~(if-then form) ~(normalize (if-else form))))
          form
        )
      )
    )
    form
  )
)

; Main functions

; Here P is a propositional expression represented as a Clojure form, like (or a (and b c)). 
; Translate P to an equivalent nested if, using rules 1 through 5, and return that if. 
; The returned if is not normalized.
(defn ifify [P]
  (cond
    (not? P) (build-not P)
    (and? P) (build-and P)
    (or? P) (build-or P)
    (imply? P) (build-imply P)
    (equiv? P) (build-equiv P)
    (if? P) `(if ~(ifify (if-test P)) ~(ifify (if-then P)) ~(ifify (if-else P))) 
    :default P
  )
)

; Here C is a nested if that might have been returned from ifify. 
; Translate C to an equivalent normalized if, using rule 6, and return that if.
(defn normalize [C] 
  (if (if? C) (normalize-if C) C)
)

; This is a helper function for simplify. Here C is a nested if, V is a symbol, and B is a 
; Boolean value (either true or false). Return C{ V ⇒ B }. 
; In other words, return a new nested if that is like C, but in which each V is replaced by B.
; Ex: (substitute '(if x a b) a true) => '(if x true b)
; Ex: (substitute '(if x (= a false) a) a true) => '(if x (= true false) true)
(defn substitute [C V B]
  (if (if? C)
    `(if ~(substitute (if-test C) V B) ~(substitute (if-then C) V B) ~(substitute (if-else C) V B))
    (if (= C V) B C) 
  )
)

(defn do-simplify[form]
  (if (if? form)
    (let [testVal (do-simplify (if-test form)) thenVal (do-simplify (if-then form)) elseVal (do-simplify (if-else form))]
      (cond
        (= testVal 'true) thenVal
        (= testVal 'false) elseVal
        :default (cond
          ; these cases handle more complex test values
          (and (= thenVal 'true) (= elseVal 'false)) testVal 
          (or (= thenVal elseVal) (tautology? `(equiv ~thenVal ~elseVal))) thenVal ; arbitrary, you can return either
          :default `(if ~testVal ~(substitute thenVal testVal 'true) ~(substitute elseVal testVal 'false)) ; keep going..
        )
      )
    )
    form
  )
)

; Here C is a normalized nested if that might have been returned from normalize. 
; Simplify C using rules 7 through 11, and return the result.
(defn simplify [C]
  (let [C' (do-simplify C)]
    (if (= C' C) C (simplify C')) ; call do-simplify until no change
  )
)

; Return true if P is a tautology (if it simplifies down to true), false otherwise
(defn tautology? [P]
  (let [simplified (simplify (normalize (ifify P)))]
    (= simplified 'true)
  )
)
