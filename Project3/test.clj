;I probably screwed up somewhere so if you find a test that's wrong post on the forum about it
(defn tester []
  (println "\n-----------------------\n")
  (println "True Tests\n")
  (if (has-type? 'x 'int {'x '(int)}) (println "TRUE TEST 1 PASSED")  (println "TRUE TEST 1 FAILED"))
  (if (has-type? 'x 'real {'x '(int real)}) (println "TRUE TEST 2 PASSED") (println "TRUE TEST 2 FAILED"))
  (if (has-type? '(+ x y) 'real {'x '(real) 'y '(real) '+ '((proc (int int) int) (proc (real real) real))}) (println "TRUE TEST 3 PASSED") (println "TRUE TEST 3 FAILED"))
  (if (has-type? '(cons (first x) (rest x)) 'list {'x '(list) 'cons  '((proc (obj list) list)) 'first '((proc (list) obj)) 'rest  '((proc (list) list))}) (println "TRUE TEST 4 PASSED") (println "TEST 4 FAILED"))
  (if (has-type? '(func a b) 'real {'a '(int) 'b '(int) 'func '((proc (int int) int))}) (println "TRUE TEST 5 PASSED") (println "TRUE TEST 5 FAILED"))
  (if (has-type? '(func a b) 'real {'a '(real) 'b '(int) 'func '((proc (real real) int))}) (println "TRUE TEST 6 PASSED") (println "TRUE TEST 6 FAILED"))
  (if (has-type? 'x 'real  {'x '(int)}) (println "TRUE TEST 7 PASSED")  (println "TRUE TEST 7 FAILED"))

  (println "\nFalse Tests\n")
  ;Wants an int returned, gets a real
  (if (has-type? 'x 'int {'x '(real)}) (println "FALSE TEST 1 FAILED") (println "FALSE TEST 1 PASSED"))
  ;x isn't in the map
  (if (has-type? 'x 'real {'y '(real)}) (println "FALSE TEST 2 FAILED") (println "FALSE TEST 2 PASSED"))
  ;The function last isn't in the map
  (if (has-type? '(cons (last x) (rest x)) 'list {'x '(list) 'cons  '((proc (obj list) list)) 'first '((proc (list) obj)) 'rest  '((proc (list) list))}) (println "FALSE TEST 3 FAILED") (println "FALSE TEST 3 PASSED"))
  ;subtract not in the map
  (if (has-type? '(- x y) 'real {'x '(real) 'y '(real) '+ '((proc (int int) int) (proc (real real) real))}) (println "FALSE TEST 4 FAILED") (println "FALSE TEST 4 PASSED"))
  ;Wants an int to be returned, gets a real
  (if (has-type? '(func a b) 'int {'a '(int) 'b '(int) 'func '((proc (int int) real))}) (println "FALSE TEST 5 FAILED") (println "FALSE TEST 5 PASSED"))
  ;Wants two ints as paramters, but a is a real
  (if (has-type? '(func a b) 'real {'a '(real) 'b '(int) 'func '((proc (int int) int))}) (println "FALSE TEST 6 FAILED") (println "FALSE TEST 6 PASSED"))
  ;func2 only given one parameter, wants 2
  (if (has-type? '(func (func2 a) b) 'real {'a '(int) 'b '(int) 'func '((proc (int int) real)) 'func2 '((proc (int int) int))}) (println "FALSE TEST 7 FAILED") (println "FALSE TEST 7 PASSED"))


  (println "\n-----------------------\n")
)
