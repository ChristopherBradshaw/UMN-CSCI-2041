; Constants
(def epsilon 0.000001)

; Recursive functions
(def _exponential
  (fn [x F S T]
    (if
      (<= T epsilon)
      S
      (let
        [newF (+ F 1.0)
         newS (+ S T)
         newT (* T (/ x F))]
      (recur x newF newS newT)))))

(def _square-root
  (fn [x G H]
    (if
      (< (Math/abs (- G H)) epsilon)
      G
      (let
        [newG (/ (+ G H) 2.0)
         newH (/ x newG)]
      (recur x newG newH)))))


; Wrapper functions

; Assume that x is a real number.
; This function assumes negative numbers are real numbers.
(def exponential
  (fn [x]
    (if 
      (< x 0)
      (let 
        [x (* x -1)]
      (/ 1.0 (_exponential x 1.0 0.0 1.0)))
      (_exponential x 1.0 0.0 1.0)))) 


; Assume that x is a non-negative real number.
; This function will return nil if a negative number is supplied
(def square-root
  (fn [x]
    (if
      (< x 0)
      nil
      (_square-root x x 1.0))))
