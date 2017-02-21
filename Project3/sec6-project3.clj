; BONUS POINTS FOR MIDTERM 2


; !!!!!!!!!!!!!! READ ME !!!!!!!!!!!!!!!!!
;
; Consider the following test case: (has-type? '(func a b) 'real {'a '(real) 'b '(int) 'func '((proc (real real) int))})
; the lab specs make it seem like this should be invalid (since (real int) isn't exactly (real real)), but a TA said in the discussion forums that this
; (among other test cases) is valid, so I changed my code to reflect this.
;
; If this is wrong (types must match EXACTLY, not even subtypes), look at "SEE ME"s to see the fix



(defn build-body [f1 forms]
  (if (empty? forms)
    (list f1)
    (concat (list `(not ~f1)) (build-body (first forms) (rest forms)))))

; Ex: (macroexpand-1 '(imply a b c d)) => (or (not a) (not b) (not c) d)
; Ex: (macroexpand-1 '(imply a)) => a
; Ex: (imply (= 5 5) false) => false
; Ex: (imply (= 5 5) true) => true 
; Ex: (imply (= 5 5) (= 5 4) false) => true
; Ex: (imply (= 5 5) (= 5 4) true) => true
(defmacro imply [f1 & forms]
  (if (or (= nil forms) (empty? forms))
    f1
    `(or ~@(build-body f1 forms))
  )
)

;
;  TOYTYPE. A toy type system.
;
;    James Moen
;    02 Dec 16
;
;  This is the toy type system from the lectures, with a few changes. The ARRAY
;  type is missing, because we won't need it for Project 3.  A new type OBJ has
;  also been added; all types are subtypes of OBJ.
;

;  Yadda yadda yadda, so we can define things in a readable order.

(def error)
(def pairwise)
(def proc-parameters)
(def proc-result)
(def proc-subtype?)
(def proc-type?)
(def simple-subtype?)
(def simple-type?)
(def subtype?)
(def has-type?)


; Return true if e ∈ t (e has the type t), and false otherwise. 
; In particular, return false if any function in e is called with arguments of unexpected types. 
(def has-type? 
  (fn [e t m]
    (cond
      ; we might be dealing with a plain symbol
      (not (seq? e))
      (cond
        (simple-type? e)
        (let [value (get m e)]
          (if (simple-type? value)
            ; SEE ME - this would be (= value t) instead
            (subtype? value t)
            ; SEE ME - the body of this if would be (some (partial = t) value) true false) instead
            (if
              (loop [current-value value]
                (if (or (= nil current-value) (empty? current-value))
                  false
                  (if (subtype? (first current-value) t) 
                    true
                    (recur (rest current-value))
                  )
                )
              )
              true
              false
            )
          )
        )
        :default false
      )
      ; there's a list of symbols/expressions
      (not (empty? e))
      (loop [fn-types (get m (first e))]
        (if (or (= nil fn-types) (empty? fn-types))
          false
          (if 
            (and 
              (proc-type? (first fn-types))
              ; is the proc type a subtype of our desired type?
              (subtype? (nth (first fn-types) 2) t)
              ; are the number of parameters the same?
              (= (count (nth (first fn-types) 1)) (count (rest e)))
              ; does each corresponding argument have the same type?
              (every? true? (map (fn [a b] (has-type? a b m)) (rest e) (nth (first fn-types) 1)))
            )
            true
            (recur (rest fn-types))
          )
        )
      )
      :default false
    )
  )
)

;  SIMPLE-SUBTYPES. Assert that simple subtypes are subtypes of each other.
;
;    t ⊆ complex   t ⊆ real   t ⊆ int
;    ───────────   ────────   ────────
;     t ⊆ real     t ⊆ int    t ⊆ bool

(def simple-subtypes
 (hash-map
   'bool  #{'int 'real 'complex}
   'int   #{'real 'complex}
   'real  #{'complex}))

;  SUBTYPE?. Test if LEFT-TYPE is a subtype of RIGHT-TYPE.
;
;     t ⊆ t     t ⊆ obj
;    ───────    ───────    
;     true       true
(def subtype?
 (fn [left-type right-type]
  (cond
   (= left-type right-type)
   true

   (= right-type 'obj)
   true

   (simple-type? left-type)
   (simple-subtype? left-type right-type)

   (proc-type? left-type)
   (proc-subtype? left-type right-type)

   true
   (error "Type expected."))))

;  SIMPLE-TYPE?. Test if an object is a simple type.

(def simple-type? symbol?)

;  SIMPLE-SUBTYPE?. Test if simple type LEFT-TYPE is a subtype of RIGHT-TYPE.

(def simple-subtype?
 (fn [left-type right-type]
  (and
   (simple-type? right-type)
   (contains? (get simple-subtypes left-type) right-type))))

;  LIST-TYPE?. Test if TYPE is a list type.

(def list-type?
 (fn [type]
  (and
   (not (empty? type))
   (= (first type) 'list))))

;  PROC-TYPE?. Test if TYPE is a PROCedure type.

(def proc-type?
 (fn [type]
  (and
   (seq? type)
   (not (empty? type))
   (= (first type) 'proc))))

;  PROC-SUBTYPE?. Test if PROCedure type LEFT-TYPE is a subtype of RIGHT-TYPE.
;
;    proc (t₁ t₂ ... tⱼ) t ⊆ proc (T₁, T₂ ..., Tⱼ) T
;    ───────────────────────────────────────────────
;       T₁ ⊆ t₁,  T₂ ⊆ t₂  ...,  Tⱼ ⊆ tⱼ,  t ⊆ T

(def proc-subtype?
 (fn [left-type right-type]
  (and
   (proc-type? right-type)
   (subtype?
    (proc-result left-type)
    (proc-result right-type))
   (pairwise
    (fn [left-parameter right-parameter]
     (subtype? right-parameter left-parameter))
    (proc-parameters left-type)
    (proc-parameters right-type)))))

;  PROC-RESULT. Return the result type of a PROCedure type.

(def proc-result (comp first rest rest))

;  PROC-PARAMETERS. Return the list of parameter types from a PROCedure type.

(def proc-parameters (comp first rest))

;  PAIRWISE. Test if each element of LEFTS, and each positionally corresponding
;  element of RIGHTS, satisfies the 2-ary predicate TEST?.

(def pairwise
 (fn [test? lefts rights]
  (if
   (or
    (empty? lefts)
    (empty? rights))
   (and
    (empty? lefts)
    (empty? rights))
   (and
    (test?
     (first lefts)
     (first rights))
    (recur test?
     (rest lefts)
     (rest rights))))))

;  ERROR. Assert that an error has occurred.

(def error
 (fn [message]
  (throw (Exception. message))))
