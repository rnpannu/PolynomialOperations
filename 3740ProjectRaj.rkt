#lang racket

; is-dense, problem is the same as ensuring the list is a list of numbers.
(define (is-dense? list)
  (cond
    [(null? list) #t]
    [(number? (first list)) (is-dense? (rest list))]
    [else #f]))

; is-sparse, if we break down the entries into pairs using a helper function we can use the same approach as is-dense.
(define (pair? list)
  (and (list? list) (and (is-dense? list) (= (length list) 2))))

(define (is-sparse? list)
  (cond
    [(null? list) #t]
    [(pair? (first list)) (is-sparse? (rest list))]
    [else #f]))

;to-sparse, utilizes a helper function that takes in an integer representing the base offset from the beginning
;           as the list gets smaller with recursion. To convert from dense, it cons's a pair of the first element
;           /coefficient and its index/power, then recurses down the list until the base case of null.
(define (to-sparse lst)
  (to-sparse-helper lst 0))

(define (to-sparse-helper lst offset)
  (cond
    [(null? lst) '()]
    [(is-sparse? lst) lst]
    [else (cons (list (first lst) (+ (index-of lst (first lst)) offset)) (to-sparse-helper (rest lst) (+ offset 1)))]))

;to-dense, utilizes a helper function that takes in an integer representing the power of the current term in the
;          dense representation of the polynomial. If the current pair's power in the sparse representation matches
;          that index cons's the coffecient of that pair into the dense representation, otherwise cons 0 and continue.

(define (to-dense lst)
  (to-dense-helper lst 0))

(define (to-dense-helper lst index)
  (cond
    [(null? lst) '()]
    [(is-dense? lst) lst]
    [(= (first(rest(first lst))) index) (cons (first (first lst)) (to-dense-helper (rest lst) (+ index 1)))]
    [else (cons 0 (to-dense-helper lst (+ index 1)))]))

;degree, simply increments an integer for each index traversed in the dense representation of a polynomial

(define (degree-helper lst)
  (cond
    [(empty? lst) -1]
    [(cons? lst) (+ 1 (degree-helper (rest lst)))]))

 
(define (degree lst)
  (cond
    [(is-sparse? lst) (degree-helper (to-dense lst))]
    [else (degree-helper lst)]))

;is-zero, recurses down an increasingly smaller list checking if the first element is = 0. Will only return true if
;         it reaches the end (if every element is 0).

(define (is-zero-helper lst)
  (cond
    [(null? lst) #t]
    [(= (first lst) 0) (is-zero-helper (rest lst))]
    [else #f]))
  ;(and (= (degree lst) 0) (= 0 (first lst))))

(define (is-zero? lst)
  (cond
    [(is-sparse? lst) (is-zero-helper (to-dense lst))]
    [else (is-zero-helper lst)]))

;coeff, utilizs a helper function that checks if the index (power) of the first/current term in the dense represen
;       -tation of the list matches the k parameter. Otherwise recurses down the list, decrementing k to account
;       for the smaller list size/degree.

(define (coeff-helper lst k)
  (cond
    [(null? lst) 0]
    [(= k (index-of lst (first lst))) (first lst)]
    [else (coeff-helper (rest lst) (- k 1))]))

(define (coeff lst k)
  (cond
    [(is-sparse? lst) (coeff-helper (to-dense lst) k)]
    [else (coeff-helper lst k)]))

;eval, utilizes a helper function that takes in an integer parameter that stores the offset from the beginning of
;      the array, this represents the power of the current term when that information is not accessible from the
;      smaller sub-lists during the recursion down the list. Simply multiplies the coefficient of the current
;      by k ^ offset and recurses, incrementing offset.

(define (eval lst k)
  (cond
    [(is-sparse? lst) (eval-helper (to-dense lst) k 0)]
    [else (eval-helper lst k 0)]))

(define (eval-helper lst k offset)
  (cond
    [(null? lst) 0]
    [(+ (* (first lst) (expt k offset)) (eval-helper (rest lst) k (+ offset 1)))]))

;add, utilizes a helper function 
(define (add p1 p2)
  (cond
    [(and (is-sparse? p1) (is-sparse? p2)) (to-sparse (cutoff-zeros (add-helper (to-dense p1) (to-dense p2))))]
    [(and (is-dense? p1) (is-sparse? p2)) (cutoff-zeros (add-helper p1 (to-dense p2)))]
    [(and (is-sparse? p1) (is-dense? p2)) (cutoff-zeros (add-helper (to-dense p1) p2))]
    [else (cutoff-zeros (add-helper p1 p2))]))

(define (add-helper p1 p2)
  (cond
    [(and (null? p1) (null? p2) '())]
    [(null? p1) (cons (first p2) (add-helper p1 (rest p2)))]
    [(null? p2) (cons (first p1) (add-helper (rest p1) p2))]
    [else (cons (+ (first p1) (first p2)) (add-helper (rest p1) (rest p2)))]))


(define (subtract p1 p2)
  (cond
    [(and (is-sparse? p1) (is-sparse? p2)) (to-sparse (cutoff-zeros (subtract-helper (to-dense p1) (to-dense p2))))]
    [(and (is-dense? p1) (is-sparse? p2)) (cutoff-zeros (subtract-helper p1 (to-dense p2)))]
    [(and (is-sparse? p1) (is-dense? p2))(cutoff-zeros (subtract-helper (to-dense p1) p2))]
    [else (cutoff-zeros (subtract-helper p1 p2))]))

(define (subtract-helper p1 p2)
  (cond
    [(and (null? p1) (null? p2) '())]
    [(null? p1) (cons (first p2) (subtract-helper p1 (rest p2)))]
    [(null? p2) (cons (first p1) (subtract-helper (rest p1) p2))]
    [else (cons (- (first p1) (first p2)) (subtract-helper (rest p1) (rest p2)))]))

(define (multiply p1 p2)
  (cond
    [(and (is-sparse? p1) (is-sparse? p2)) (to-sparse (cutoff-zeros (multiply-helper (to-dense p1) (to-dense p2) 0)))]
    [(and (is-dense? p1) (is-sparse? p2)) (cutoff-zeros (multiply-helper p1 (to-dense p2) 0))]
    [(and (is-sparse? p1) (is-dense? p2)) (cutoff-zeros (multiply-helper (to-dense p1) p2 0))]
    [else (cutoff-zeros (multiply-helper p1 p2 0))]))

(define (multiply-helper p1 p2 offset)
  (cond
    [(null? p1) (distribute 0 offset p2)]
    [else (add (distribute (first p1) offset p2) (multiply-helper (rest p1) p2 (+ offset 1)))]))

(define (distribute scalar power lst)
  (cond
    [(null? lst) '()]
    [(> power 0) (cons 0 (distribute scalar (- power 1) lst))]
    [else (cons (* scalar (first lst)) (distribute scalar power (rest lst)))]))

(define (quotient p1 p2)
  (cond
    [(and (is-sparse? p1) (is-sparse? p2)) (to-sparse (quotient-helper (to-dense p1) (to-dense p2)))]
    [(and (is-dense? p1) (is-sparse? p2)) (quotient-helper p1 (to-dense p2))]
    [(and (is-sparse? p1) (is-dense? p2))(quotient-helper (to-dense p1) p2)]
    [else (quotient-helper p1 p2)]))

(define (quotient-helper p1 p2)
  (cond
     [(> (degree p2) (degree p1)) '(0)]
     [(is-zero? p1) '(0)]
     [(is-zero? (findcoeff p1 p2 0)) '(0)]
     [else (add (findcoeff p1 p2 0) (quotient-helper (subtract p1  (multiply (findcoeff p1 p2 0) p2)) p2))]))

(define (findcoeff p1 p2 offset)
  (cond
    [(= offset (- (degree p1)  (degree p2))) (list (quotient (coeff p1 (degree p1)) (coeff p2 (degree p2))))]; have power of quotient term
    [else (cons 0 (findcoeff p1 p2 (+ offset 1)))]))

(define (cutoff-zeros lst)
  (cond
    [(= (last lst) 0) cutoff-zeros(reverse (rest (reverse lst)))]
    [else lst]))
  

(define T1 '((1 0) (1 1)))                   ; (x+1)                  sparse
(define T2 '(1 2 1))                         ; (x+1)^2                dense
(define T3 '((1 0) (3 1) (3 2) (1 3)))       ; (x+1)^3                sparse
(define T4 '(2 3 1))                         ; (x+1)(x+2)             dense
(define T5 '((4 1) (1 2)))                   ; x(x+4)                 sparse 
(define T6 '(0 0 1))                         ; x^2                    dense
(define T7 '((-1 0) (-1 1) (12 2)))          ; (3x - 1)(4x + 1)       sparse
(define T8 '(-1 2 1))                        ; x^2 + 2x -1            dense
(define T9 '((7 0) (1 1) (-1 2)))            ; -x^2 + x + 7           sparse
(define T10 '(1))                            ; 1                      dense
(define T11 '((0 0)))                        ; 0                      sparse
(define T12 '(1 0 1))                        ; x^2 + 1                dense

(displayln "initial Test Cases")
T1
T2
T3
T4
T5
T6
T7
T8
T9
T10
T11
T12


(displayln "")
(displayln "is-sparse? test")

(is-sparse? T1)
(is-sparse? T2)
(is-sparse? T3)
(is-sparse? T4)
(is-sparse? T5)
(is-sparse? T6)
(is-sparse? T7)
(is-sparse? T8)
(is-sparse? T9)
(is-sparse? T10)
(is-sparse? T11)
(is-sparse? T12)



(displayln "")
(displayln "is-dense? test")

(is-dense? T1)
(is-dense? T2)
(is-dense? T3)
(is-dense? T4)
(is-dense? T5)
(is-dense? T6)
(is-dense? T7)
(is-dense? T8)
(is-dense? T9)
(is-dense? T10)
(is-dense? T11)
(is-dense? T12)


(displayln "")
(displayln "to-sparse test")
(to-sparse T1)
(to-sparse T2)
(to-sparse T3)
(to-sparse T4)
(to-sparse T5)
(to-sparse T6)
(to-sparse T7)
(to-sparse T8)
(to-sparse T9)
(to-sparse T10)
(to-sparse T11)
(to-sparse T12)


(displayln "")
(displayln "to-dense test")
(to-dense T1)
(to-dense T2)
(to-dense T3)
(to-dense T4)
(to-dense T5)
(to-dense T6)
(to-dense T7)
(to-dense T8)
(to-dense T9)
(to-dense T10)
(to-dense T11)
(to-dense T12)


(displayln "")
(displayln "degree test")
(degree T1)
(degree T2)
(degree T3)
(degree T4)
(degree T5)
(degree T6)
(degree T7)
(degree T8)
(degree T9)
(degree T10)
(degree T11)
(degree T12)


(displayln "")
(displayln "is-zero? test")
(is-zero? T1)
(is-zero? T2)
(is-zero? T3)
(is-zero? T4)
(is-zero? T5)
(is-zero? T6)
(is-zero? T7)
(is-zero? T8)
(is-zero? T9)
(is-zero? T10)
(is-zero? T11)
(is-zero? T12)



(displayln "")
(displayln "coef test")
(coeff T1 1) 
(coeff T2 2) 
(coeff T3 3) 
(coeff T4 0) 
(coeff T5 0) ; illegal case? should be 0 
(coeff T6 2) 
(coeff T7 1) 
(coeff T8 2) 
(coeff T9 2) 
(coeff T10 0)
(coeff T11 0)
(coeff T12 1)


(displayln "")
(displayln "Eval test")
(eval T1 0) 
(eval T2 1) 
(eval T3 2) 
(eval T4 3) 
(eval T5 4) 
(eval T6 -1) 
(eval T7 0) 
(eval T8 100) 
(eval T9 3) 
(eval T10 100)
(eval T11 12)
(eval T12 2)

(displayln "")
(displayln "add test")
(add T1 T1)
(add T2 T1)
(add T3 T1)
(add T4 T11)
(add T5 T10)
(add T6 T6)
(add T7 T2)
(add T8 '(0 -2))
(add T9 T8)
(add T10 T10)
(add T11 T11)
(add T12 T12)

(displayln "")
(displayln "subtract test")
(subtract T1 T1)
(subtract T2 T1)
(subtract T3 T11)
(subtract T4 T2)
(subtract T5 T3)
(subtract T6 T8)
(subtract T7 T1)
(subtract T8 T9)
(subtract T9 T8)
(subtract T10 T10)
(subtract T11 T6)
(subtract T12 T12)

(displayln "")
(displayln "special test")
(add
'(0 0 1)
'(0 -1 -1)
)



(displayln "")
(displayln "multiply test")
(multiply T1 T2)
(multiply T2 T2)
(multiply T3 T10)
(multiply T4 T11)
(multiply T5 T1)
(multiply T6 T3)
(multiply T7 T1)
(multiply T8 T9)
(multiply T9 T8)
(multiply T10 T11)
(multiply T11 T3)
(multiply T12 T12)
(multiply T11 T1)


(displayln "")
(displayln "division test")
(quotient T1 T1)
(quotient T2 T1)
(quotient T3 T2)
(quotient T4 T1)
(quotient T5 T7)
(quotient T6 T1)
(quotient T7 T10) 
(quotient T8 T11)
(quotient T9 T6)
(quotient T10 T10)
(quotient T11 T12)
(quotient T12 T1)
(quotient '(5) T10)
