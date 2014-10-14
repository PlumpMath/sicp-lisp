#! /usr/bin/env python3
from lisp import run

fib = """
(define (fib n)
  (let fib-iter ((a 1) (b 0) (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) 
                  a 
                  (- count 1)))))

(fib 3)
"""

fact = """
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

(factorial 3)
"""

unless = """
(define (unless condition 
                usual-value 
                exceptional-value)
  (if condition 
      exceptional-value 
      usual-value))

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

(factorial 3)
"""

run(unless)
