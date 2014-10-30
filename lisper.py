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

amb_base = """
(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) 
       (an-element-of (cdr items))))
"""

amb_prime_sum = amb_base + """
(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))

(prime-sum-pair '(1 3 5 8) '(20 35 110))
"""

amb_pythag = amb_base + """
(define (an-integer-between low high)
  (require (< low high))
  (amb low
       (an-integer-between (+ low 1) high) ))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) 
                    (* k k)))
        (list i j k)))))

(a-pythagorean-triple-between 3 4)

try-again
"""

run(amb_pythag)
