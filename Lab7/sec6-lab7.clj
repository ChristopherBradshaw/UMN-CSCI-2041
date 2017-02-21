
; PART A

; (which (+ 3 2) (+ 2 3) "A" (+ 1 2) "B") => "A"
; -->
; (let [S' 5]
;   (cond
;     (= S' 5) "A"
;     (= S' 3) "B"
;     :else "C"
;   )
; )

(defn build-cond-body [S forms] 
  (loop [args forms retList '()]
    (if (empty? args)
      retList
      (recur (rest (rest args)) (concat retList (list `(= ~S ~(eval (first args)))) (list (eval (second args)))))
    )
  )
)

(defmacro which [S & args]
  (let [S' (eval S)]
    `(cond
      ~@(build-cond-body S' args) 
    )
  )
)

; PART B

; assumed that (most) => false and (most a) => a
;
; (most (= 5 (+ 1 4)) (= 4 4) (= 4 3)) => true
; -->
; (> (+ (if (= 5 (+ 1 4)) 1 0) (if (= 4 4) 1 0) (if (= 4 3) 1 0)) 1)

(defn build-most-body [forms]
  (map (fn [a] `(if ~a 1 0)) forms)
)

(defmacro most [& args] 
  (let [threshold (int (/ (count args) 2))]
    `(> (+ ~@(build-most-body args)) ~threshold)
  )
) 

; PART C
; (comps inc 3) => (let [g inc] (fn [x] (g (g (g x))))) 
; ((comps inc 3) 1) => ((let [g inc] (fn [x] (g (g (g x))))) 1) => 4

(defn build-comps-body [F n]
  (loop
    [L '(comp) n' n]
    (if (= n' 0)
      L
      (recur (concat L (list F)) (- n' 1))
    )
  )
)

(defmacro comps [F n]
  ;`(let [g# ~F] (build-comps-body g# n))
  `(let [g# ~F] 
    ~(cond 
      (= n 0) '(fn [x] x)
      (= n 1) F
      :default (build-comps-body F n)
    )
  )
)


; PART D
; (qrat + 1.0 3.0 2.0) => (let [a# 1 b# 3 c# 2 rt (Math/sqrt (- (* b# b#) (* 4 a# c#))] (/ (S (- b) rt) (* 2 a#))) 

(defn det[a b c]
  (Math/sqrt (- (* b b) (* 4 a c)))
)

(defmacro qrat [S a b c]
  `(let [s# ~S a# ~(eval a) b# ~(eval b) c# ~(eval c)]
    (/ (s# (- b#) (det a# b# c#)) (* 2 a#))
  )
)
