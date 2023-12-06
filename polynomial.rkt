#lang racket

;Preamble: Nearly every function that matches the names of the ones defined
;          in the project handout only performs the job of interpreting the
;          representation(s) of the input polynomial(s) in order to:
;                - Determine the representation of the output polynomial
;                - Convert the representation(s) of the inputs if needed
;                  before passing them as parameters to one or more helper functions.

;          All of the internal logic is done on polynomials of dense representation.
;          All of the logic is in the helper functions.
;          For ease of reading the interpreting function is always
;          the first one for that operation.


;cutoff-zeros, a general helper function that strips all trailing 0's off the polynomial.
;              Last is 0 and is trailing? -> reverse rest reverse (1 2 0) -> (0 2 1) -> (2 1) -> (1 2)

(define (cutoff-zeros lst)
  (cond
    [(and (= (last lst) 0) (not (null? (rest lst)))) (cutoff-zeros(reverse (rest (reverse lst))))]
    [(null? lst) '()]
    [else lst]))


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


;to-sparse, utilizes a helper function that takes in an integer representing the offset from the beginning of the list 
;           as the list gets smaller with recursion. To convert from dense, it cons's a pair of the first element
;           /coefficient and its index/power, then recurses down the list until the base case of null.

(define (to-sparse lst)
  (to-sparse-helper lst 0))

(define (to-sparse-helper lst offset)
  (cond
    [(null? lst) '()]
    [(is-sparse? lst) lst]
    [(and (= offset 0) (= 0 (first lst)) (= 1 (length lst))) '((0 0))]; 0 polynomial case
    [(= 0 (first lst)) (to-sparse-helper (rest lst) (+ offset 1))]; skip powers with coefficient 0
    [else (cons (list (first lst) offset) (to-sparse-helper (rest lst) (+ offset 1)))]))


;to-dense, utilizes a helper function that takes in an integer representing the power of the current term in the
;          dense representation of the polynomial. If the current pair's power in the sparse representation matches
;          that index, cons's the coffecient of that pair into the dense representation, otherwise cons 0 and continue.

(define (to-dense lst)
  (to-dense-helper lst 0))

(define (to-dense-helper lst index)
  (cond
    [(null? lst) '()]
    [(is-dense? lst) lst]
    [(= (first(rest(first lst))) index) (cons (first (first lst)) (to-dense-helper (rest lst) (+ index 1)))]
    [else (cons 0 (to-dense-helper lst (+ index 1)))]))


;degree, simply increments an integer for each index traversed in the dense representation of a polynomial

(define (degree lst)
  (cond
    [(is-zero? lst) -inf.0]
    [(is-sparse? lst) (degree-helper (to-dense lst))]
    [else (degree-helper lst)]))

(define (degree-helper lst)
  (cond
    [(empty? lst) -1]
    [(cons? lst) (+ 1 (degree-helper (rest lst)))]))


;is-zero, recurses down an increasingly smaller list checking if the first element is = 0. Will only return true if 
;         it reaches the end (if every element is 0).

(define (is-zero? lst)
  (cond
    [(is-sparse? lst) (is-zero-helper (to-dense lst))]
    [else (is-zero-helper lst)]))

(define (is-zero-helper lst)
  (cond
    [(null? lst) #t]
    [(= (first lst) 0) (is-zero-helper (rest lst))]
    [else #f]))



;coeff, utilizs a helper function that checks if the index (power) of the first/current term in the dense represen
;       -tation of the list matches the k parameter. Otherwise recurses down the list, decrementing k to account
;       for the smaller list size/degree.

(define (coeff lst k)
  (cond
    [(is-sparse? lst) (coeff-helper (to-dense lst) k)]
    [else (coeff-helper lst k)]))
(define (coeff-helper lst k)
  (cond
    [(null? lst) 0]
    [(= k (index-of lst (first lst))) (first lst)]
    [else (coeff-helper (rest lst) (- k 1))]))


;eval, utilizes a helper function that takes in an integer parameter that stores the offset from the beginning of
;      the array, this represents the power of the current term when that information is not accessible from the
;      smaller sub-lists during the recursion down the list.
;      Simply multiplies the coefficient of the current first term by k ^ offset and recurses, incrementing offset.

(define (eval lst k)
  (cond
    [(is-sparse? lst) (eval-helper (to-dense lst) k 0)]
    [else (eval-helper lst k 0)]))

(define (eval-helper lst k offset)
  (cond
    [(null? lst) 0]
    [(+ (* (first lst) (expt k offset)) (eval-helper (rest lst) k (+ offset 1)))]))


;add, utilizes a helper function that cons's the sum of coeffecients of two terms with equivalent power.
;     if one polynomial is larger than the other, cons's the terms of the remaining one.

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


;subtract, same logic as add function but subtracting coefficients instead.
(define (subtract p1 p2)
  (cond
    [(and (is-sparse? p1) (is-sparse? p2)) (to-sparse (cutoff-zeros (subtract-helper (to-dense p1) (to-dense p2))))]
    [(and (is-dense? p1) (is-sparse? p2)) (cutoff-zeros (subtract-helper p1 (to-dense p2)))]
    [(and (is-sparse? p1) (is-dense? p2))(cutoff-zeros (subtract-helper (to-dense p1) p2))]
    [else (cutoff-zeros (subtract-helper p1 p2))]))

(define (subtract-helper p1 p2)
  (cond
    [(and (null? p1) (null? p2) '())]
    [(null? p1) (cons (- (first p2)) (subtract-helper p1 (rest p2)))]
    [(null? p2) (cons (first p1) (subtract-helper (rest p1) p2))]
    [else (cons (- (first p1) (first p2)) (subtract-helper (rest p1) (rest p2)))]))


; multiply, utilizes two helper functions that execute two subproblems. Distribute performs the task of multiplying 
;           a densely represented polynomial by a scalar coefficient, and multiply-helper utilizes the previously 
;           defined add function to add successive distributions (done for each element of p1 by recursively calling
;           multiply helper on rest p1).

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


;quotient,  utilizes two helper functions that execute two subproblems. Findcoeff performs the task of finding a  
;           a quotient term of sufficient degree and coefficient to cancel the highest degree term of p1, and
;           quotient-helper utilizes the previously defined add function to add successive quotient terms of
;           decreasing power, found by calling findcoeff on the list returned by subtracting p1 by (q0 * p2), where
;           q0 is the previous quotient term found by findcoeff and p2 is the divisor.

(define (quotient p1 p2)
  (cond
    [(and (is-sparse? p1) (is-sparse? p2)) (to-sparse (cutoff-zeros (quotient-helper (to-dense p1) (to-dense p2))))]
    [(and (is-dense? p1) (is-sparse? p2)) (cutoff-zeros (quotient-helper p1 (to-dense p2)))]
    [(and (is-sparse? p1) (is-dense? p2))(cutoff-zeros (quotient-helper (to-dense p1) p2))]
    [else (cutoff-zeros (quotient-helper p1 p2))]))

(define (quotient-helper p1 p2)
  (cond
     [(> (degree p2) (degree p1)) '(0)]
     [(is-zero? p1) '(0)]
     [(is-zero? p2) '(0)]
     [(is-zero? (findcoeff p1 p2 0)) '(0)]
     [else (add (findcoeff p1 p2 0) (quotient-helper (subtract p1  (multiply (findcoeff p1 p2 0) p2)) p2))]))

(define (findcoeff p1 p2 offset)
  (cond
    ; Are we at the quotient power needed to cancel the highest degree element of p1?
    ; Yes-> Return a list of the required coefficient of that cancelling term.
    ; No -> Fill in lower power terms with 0 and cons until recursion reaches base case.
    [(is-zero? p1) '(0)]
    [(= offset (- (degree p1)  (degree p2))) (list (/ (coeff p1 (degree p1)) (coeff p2 (degree p2))))]; (truncate (/ (coeff p1 (degree p1)) (coeff p2 (degree p2))))) 
    [else (cons 0 (findcoeff p1 p2 (+ offset 1)))]))


;remainder, the same as quotient except upon finding no possible multiple of p2 to subtract from
;           p1, returns p1 (the current dividend at that stage in the subtraction).

(define (remainder p1 p2)
  (cond
    [(and (is-sparse? p1) (is-sparse? p2)) (to-sparse (cutoff-zeros (remainder-helper (to-dense p1) (to-dense p2))))]
    [(and (is-dense? p1) (is-sparse? p2)) (cutoff-zeros (remainder-helper p1 (to-dense p2)))]
    [(and (is-sparse? p1) (is-dense? p2))(cutoff-zeros (remainder-helper (to-dense p1) p2))]
    [else (cutoff-zeros (remainder-helper p1 p2))]))

(define (remainder-helper p1 p2)
  (cond
    [(> (degree p2) (degree p1)) p1]
    [(is-zero? p1) '(0)]
    [(is-zero? p2) '(0)]
    [(is-zero? (findcoeff p1 p2 0)) p1]
    [else (remainder-helper (subtract p1  (multiply (findcoeff p1 p2 0) p2)) p2)]))


; derivative, returns the derivative of the parameter polynomial by dropping the first element in the list (constant
;             term), and continuing with the rest of the list, effectively dropping the power of every element by 1.
;             multiplies the coefficient by an integer offset parameter that represents the former power of the 
;             term with respect to the original polynomial.

(define (derivative lst)
  (cond
    [(is-sparse? lst) (to-sparse(cutoff-zeros (derivative-helper (to-dense lst) 0)))]
    [else (cutoff-zeros (derivative-helper lst 0))]))

(define (derivative-helper lst offset)
  (cond
    [(null? lst) '(0)]
    [(= 0 offset) (derivative-helper (rest lst) (+ offset 1))]
    [else (cons (* offset (first lst)) (derivative-helper (rest lst) (+ offset 1)))]))


;gcd, the Euclidean algorithm, utilizing previously defined functions. Namely taking the remainder with the polynomials
;     alternating as dividends and divisor, and distribute to make the leading coefficient 1.

(define (gcd p1 p2)
  (cond
    [(and (is-sparse? p1) (is-sparse? p2)) (to-sparse (cutoff-zeros (gcd-helper (to-dense p1) (to-dense p2))))]
    [(and (is-dense? p1) (is-sparse? p2)) (cutoff-zeros (gcd-helper p1 (to-dense p2)))]
    [(and (is-sparse? p1) (is-dense? p2))(cutoff-zeros (gcd-helper (to-dense p1) p2))]
    [else (cutoff-zeros (gcd-helper p1 p2))]))

(define (gcd-helper p1 p2)
  (cond
    [(is-zero? p2) (distribute (/ 1 (coeff p1 (degree p1))) 0 p1)]
    [else (gcd p2 (remainder p1 p2))]))

