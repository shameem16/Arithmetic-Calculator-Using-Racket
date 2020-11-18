#lang racket


(define e  2.7182818284590452353602874713527)

(define (comb n r)
  ( cond ((= n 0) "combinations is not possible as n=0")
         ((< n r) "combinations is not possible as n<r")
         ((negative-integer? n) "combinations is not possible as n<0")
         ((negative-integer? r) "combinations is not possible as r<0")
         (else (/ (fact 1 1 n) (* (fact 1 1 r) (fact 1 1 (- n r))))))) 
(define (perm n r)
  ( cond  ((= n 0) "combinations is not possible as n<0")
         ((< n r) "combination not possible as r is greater than n")
         ((negative-integer? n) "combination not possible as n<0")
         ((negative-integer? r) "combination is not possible as r<0")
         (else (/ (fact 1 1 n) (fact 1 1 (- n r))))))  

(define (fact product counter max-count)
(if (> counter max-count) product
(fact (* counter product) (+ counter 1) max-count)))

(define (expt result base pow)
(cond ((= pow 1) result)
      ((= pow 0) 1)
      (else (expt (* result base) base (- pow 1)))))

(define (exp n)
  (expt e e n))


  
  

 (define (apply-op op stack)
  (cond
    [(number? (string->number op))
     (cons (string->number op) stack)]
    [(equal? op "+")
     (cons (+ (first stack) (second stack)) (rest (rest stack)))]
    [(equal? op "-")
     (cons (- (first stack) (second stack)) (rest (rest stack)))]
    [(equal? op "*")
     (cons (* (first stack) (second stack)) (rest (rest stack)))]
    [(equal? op "/")
     (cons (/ (first stack) (second stack)) (rest (rest stack)))]
    [(equal? op "expt")
     (cons (expt  (first stack) (first stack) (second stack)) (rest (rest stack)))]
    [(equal? op "exp")
     (cons (exp (first stack))  (rest stack))]
    [(equal? op "fact")
     (cons (fact 1 1 (first stack))  (rest stack))]
    [(equal? op "comb")
     (cons (comb  (first stack) (second stack)) (rest (rest stack)))]
    [(equal? op "perm")
     (cons (perm  (first stack) (second stack)) (rest (rest stack)))]
    [(equal? op "sin")
     (cons (sin (first stack))  (rest stack))]
    [(equal? op "cos")
     (cons (cos (first stack))  (rest stack))]
    [(equal? op "tan")
     (cons (tan (first stack))  (rest stack))]
    [(equal? op "sqrt")
     (cons (sqrt (first stack))  (rest stack))]
    [(equal? op "log")
     (cons (log (first stack))  (rest stack))]
    [(equal? op "gcd")
     (cons (gcd  (first stack) (second stack)) (rest (rest stack)))]
    [(equal? op "lcm")
     (cons (lcm  (first stack) (second stack)) (rest (rest stack)))]
    [(equal? op "(")
     (car (cons stack '()))]
    [(equal? op ")")
     (car (cons stack '()))]
    [else
     (~a "operation '" op "' invalid")]))



(define (exec ops (stack empty) (opcount 1))
  "executes a stack based program."
  (cond
    [(empty? ops) (first stack)]
    [else
      (let ([next-stack (apply-op (last ops) stack)])
           (if (string? next-stack)
               (~a "Error at operator " opcount ": " next-stack)
               (exec (reverse (cdr (reverse ops))) next-stack (+ opcount 1))))]))


(define (split-string lst)
  (let loop ((acc '()) (current '()) (chars (string->list lst)))
    (cond ((null? chars)
           (reverse (cons (list->string (reverse current)) acc)))
          ((char=? (car chars) #\space)
           (loop (cons (list->string (reverse current)) acc)
                 '()
                 (cdr chars)))
          (else
           (loop acc
                 (cons (car chars) current)
                 (cdr chars))))))

