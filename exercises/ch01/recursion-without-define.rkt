#lang sicp

((lambda (fact n)
   (fact fact n))
 (lambda (fact n)
   (if (= n 0)
       1
       (* n (fact fact (- n 1))))) 60000)
