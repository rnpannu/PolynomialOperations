A functional language polynomial operation solver. Considers 2 different representation of polynomials,

[Video](https://youtu.be/zWnSM-yjhHY)

Dense: List of coefficients of increasing power

Sparse: 2-tuples of coefficient/power pairs.

See test cases and code documentation to view program logic.

Easiest way to run is to install Dr Racket IDE and run from there.

https://racket-lang.org/

Run with following test cases: 

(define A '((-1 0) (1 2))) ; (x^2-1)
(define B '(1 1)) ; (x+1)
(define C '(-1 1)) ; (x-1)
(define D '(-1 -1 1 1)) ;(x^3 + x^2 - x - 1)
(define E '((-4 0) (-13 1) (1 2))) ; x^2 - 13x - 4

(displayln "is-sparse?")
(is-sparse? A)
(is-sparse? B)
(is-sparse? C)
(is-sparse? D)
(is-sparse? E)

(displayln "is-dense?")
(is-dense? A)
(is-dense? B)
(is-dense? C)
(is-dense? D)
(is-dense? E)

(displayln "to-sparse")
(to-sparse A)
(to-sparse B)
(to-sparse C)
(to-sparse D)
(to-sparse E)

(displayln "to-dense")
(to-dense A)
(to-dense B)
(to-dense C)
(to-dense D)
(to-dense E)

(displayln "degree")
(degree A)
(degree B)
(degree C)
(degree D)

(displayln "is-zero?")
(is-zero? A)
(is-zero? B)
(is-zero? C)
(is-zero? D)
(is-zero? E)

(displayln "coeff")
(coeff A 1)
(coeff B 0)
(coeff C 1)
(coeff D 3)
(coeff E 1)

(displayln "eval")

(eval A 0)
(eval B 1)
(eval C 2)
(eval D 3)
(eval E 4)

(displayln "add")
(add A A) ; (2x^2 - 2)
(add B C) ; (2x)
(add D A) ; x^3 + 2x^2 -x - 2
(add A D)

(displayln "subtract")
(subtract A A) ; 0
(subtract A B) ; x^2 - x -2
(subtract D C) ; x^3 +x^2 -2x
(subtract B D) ; -x^3 -x^2 + 2x + 1
(subtract D E) ; x^3 + 12x + 3

(displayln "multiply")
(multiply B C) ; x^2 - 1
(multiply A B) ; x^3 + x^2 - x - 1
(multiply C A) ; x^3 - x^2 - x + 1
(multiply E (multiply E B)) ; x^5 - 25 x^4 + 135 x^3 + 265 x^2 + 120 x + 16
(multiply D A) ; x^5 + x^4 - 2x^3 -2x^2 + x + 1

(displayln "quotient")
(quotient A A)
(quotient A B) ; x - 1
(quotient A C) ; x + 1
(quotient D A) ; x + 1
(quotient D B) ; x^2 - 1
(quotient D E) ; x + 14
(quotient (multiply E (multiply E B)) E) ; x^3 - 12x^2 - 17x - 4

(displayln "remainder")
(remainder A A)
(remainder D C)
(remainder E A) ; -13x -3
(remainder E B) ; 10
(remainder D E) ; 185x + 55

(displayln "derivative")
(derivative A) ; 2x
(derivative B) ; 1
(derivative C) ; 1
(derivative D) ; 3x^2 + 2x - 1
(derivative E) ; 2x -13

(displayln "gcd")
(gcd A A) ; x^2 - 1
(gcd A B) ; x + 1
(gcd D A) ; x^2 - 1
(gcd D C) ; x - 1
(gcd E D) ; 1
