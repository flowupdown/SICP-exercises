#lang racket

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       0.001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(define (square x) (* x x))

;Example: arithmetic operations for rational numbers

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat1 n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat2 n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define one-half (make-rat1 1 2))
(define one-third (make-rat1 1 3))

;Exercise 2.1

(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((n (/ n g))
          (d (/ d g)))
      (if (or (and (> n 0) (> d 0))
              (and (< n 0) (< d 0)))
          (cons (abs n) (abs d))
          (cons (- (abs n)) (abs d))))))

;Exercise 2.2
(define (average x y)
  (/ (+ x y) 2))

(define (make-segment a b)
  (cons a b))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (make-point x y)
  (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (mid-point s)
  (make-point (average (x-point (start-segment s))
                       (x-point (end-segment s)))
              (average (y-point (start-segment s))
                       (y-point (end-segment s)))))
(define p11
  (make-point 2 2))
(define p22
  (make-point 4 4))
(define lp
  (make-segment p11 p22))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;Exercise 2.3

(define (make-rectangle a b c d)
  (cons (cons a b)
        (cons c d)))
(define (a-segment p) (car (car p)))
(define (b-segment p) (cdr (car p)))
(define (c-segment p) (car (cdr p)))
(define (d-segment p) (cdr (cdr p)))

(define (length-segment s)
  (sqrt (+ (square (- (x-point (start-segment s)) (x-point (end-segment s))))
           (square (- (y-point (start-segment s)) (y-point (end-segment s)))))))

(define (perimetr-rectangle p)
  (+ (length-segment (a-segment p))
     (length-segment (b-segment p))
     (length-segment (c-segment p))
     (length-segment (d-segment p))))

(define (area-rectangle p)
  (let ((a (length-segment (a-segment p)))
        (b (length-segment (b-segment p)))
        (c (length-segment (c-segment p)))
        (d (length-segment (d-segment p))))
    (if (= a b)
        (* a c)
        (* a b))))
   
(define r-a (make-point 1 1))
(define r-b (make-point 1 3))
(define r-c (make-point 4 3))
(define r-d (make-point 4 1))

(define r-s-a (make-segment r-a r-b))
(define r-s-b (make-segment r-b r-c))
(define r-s-c (make-segment r-c r-d))
(define r-s-d (make-segment r-d r-a))

(define rec1 (make-rectangle r-s-a r-s-b r-s-c r-s-d))

;Exercise 2.5

(define (power a n)
  (define (iter result n)
    (if (= n 1)
        result
        (iter (* result a) (- n 1))))
  (if (= n 0)
      1
      (iter a n)))

(define (cons1 a b)
  (* (power 2 a) (power 3 b)))

(define (car1 z)
  (define (iter n z)
    (if (= (remainder z 2) 0)
        (iter (+ n 1) (/ z 2))
        n))
  (iter 0 z))

(define (cdr1 z)
  (define (iter n z)
    (if (= (remainder z 3) 0)
        (iter (+ n 1) (/ z 3))
        n))
  (iter 0 z))
        
;Exersise 2.6

(define zero (lambda (x) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;Exrtended exercise

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval1 x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval1 x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

;Exercise 2.7

(define (make-interval a b) (cons a b))
(define (lower-bound i)
  (if (< (car i) (cdr i))
      (car i)
      (cdr i)))
(define (upper-bound i)
  (if (< (car i) (cdr i))
      (cdr i)
      (car i)))

;Exercise 2.8

(define (sub-interval x y)
  (let ((xx (lower-bound x))
        (xy (upper-bound x))
        (yx (lower-bound y))
        (yy (upper-bound y)))
    (make-interval
     (if (> xx yx)
         (- xx yx)
         (- yx xx))
     (if (> xy yy)
         (- xy yy)
         (- yy xy)))))


;Exercise 2.9

(define (make-width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;Exercise 2.10

(define (div-interval x y)
  (if (and (> (upper-bound y) 0)
           (> (lower-bound y) 0))
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))
      (error "divide is incorrect, change the values")))

;Exercise 2.11

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (if (> (* (lower-bound x) (lower-bound y) (upper-bound x) (upper-bound y)) 0)
           (make-interval p1 p4)
           (make-interval (min p1 p2 p3 p4)
                          (max p1 p2 p3 p4)))))


;Exercise 2.12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (- c (/ (* c p) 100))
                 (+ c (/ (* c p) 100))))

(define (percent i)
  (* 100 (/ (width i) (center i))))

;print interval

(define (print-interval i)
  (newline)
  (display "(")
  (display (lower-bound i))
  (display ", ")
  (display (upper-bound i))
  (display ")"))

;Exercise 2.13

(define (percent-mul-int x y)
  (percent (mul-interval x y)))

(define (center-mul x y)
  (+ (* (center x) (center y)) (* (width x) (width y))))

(define (width-mul x y)
  (+ (* (center x) (width y)) (* (center y) (width x))))

(define (percent-mul x y)
  (* 100 (/ (width-mul x y) (center-mul x y))))

;Exercise 2.14

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(define (print-center-percent i)
  (newline)
  (display "(")
  (display (center i))
  (display ", ")
  (display (percent i))
  (display "%)"))

(define int-a
  (make-center-percent 10.0 10))
(define int-b
  (make-center-percent 8.0 20))
(define int-one
  (make-center-percent 1.0 0))

;Representing sequences

(define one-through-four (list 1 2 3 4))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))

(define (length-iter items)
  (define (length-itr a count)
    (if (null? a)
        count
        (length-itr (cdr a) (+ 1 count))))
  (length-itr items 0))

(define (new-append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;Exercise 2.17

(define (last-pair list1)
  (if (null? (cdr list1))
      (car list1)
      (last-pair (cdr list1))))

;Exercise 2.18

(define (no-last list1)
  (if (null? (cdr list1))
      (cdr list1)
      (cons (car list1)
            (no-last (cdr list1)))))

(define (reverse list1)
  (if (null? list1)
      list1
      (cons (last-pair list1)
            (reverse (no-last list1)))))

;Exercise 2.19

(define (old-count-change amount) (old-cc amount 5))
(define (old-cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0))0)
        (else (+ (old-cc amount
                     (- kinds-of-coins 1))
                 (old-cc (- amount
                        (old-first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))
(define (old-first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define (first-denomination coin-values)
  (last-pair coin-values))
(define (except-first-denomination coin-values)
  (no-last coin-values))
(define (no-more? coin-values)
  (null? coin-values))

;Exercise 2.20

(define (odd? x)
  (= (remainder x 2) 1))
(define (even? x)
  (= (remainder x 2) 0))
(define (same-parity? x y)
  (or (and (odd? x) (odd? y))
      (and (even? x) (even? y))))
(define (pick-pair list1 n)
  (cond ((> n (length list1))
         (error "value out of region"))
        ((= n 1) (car list1))
        (else
         (pick-pair (cdr list1) (- n 1)))))
(define (without-picked list p)
  (define (iter counter list p)
    (cond ((null? list) null)
          ((= counter p) (iter (+ 1 counter) (cdr list) p))
          (else (cons (car list)
                (iter (+ 1 counter) (cdr list) p)))))
  (iter 1 list p))
  
(define (same-parity x . y)
  (define (iter n)
    (if (= n 0)
        null
        (if (same-parity? x (pick-pair y n))
            (cons (pick-pair y n)
                  (iter (- n 1)))
            (iter (- n 1)))))
  (cons x
        (reverse (iter (length y)))))

;line
(define (same-parity1 x . y)
  (define (iter x y)
  (if (null? y)
      x
      ((if (same-parity? x (last-pair y))
           (cons (last-pair y)
                 (same-parity x (no-last y)))
           (same-parity1 x (no-last y))))))
  (iter x y))
;line

;mapping over lists

(define (scale-list items factor)
  (if (null? items)
      null
      (cons (* (car items) factor)
            (scale-list (cdr items)
                        factor))))
(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))
(define (new-scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

;Exercise 2.21

(define (square-list items)
  (if (null? items)
      null
      (cons (square (car items))
            (square-list (cdr items)))))
(define (new-square-list items)
  (map (lambda (x) (square x))
       items))

;Exercise 2.22

(define (iter-square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items null))

(define (new-iter-square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items null))
      
(define (my-iter-square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (no-last things)
              (cons (square (last-pair things))
                    answer))))
  (iter items null))

;Exercise 2.23

(define (for-each proc list1)
  (proc (car list1))
  (if (null? (cdr list1))
      (newline)
      (for-each proc (cdr list1))))

;Exercise 2.27

(define (deep-reverse list1)
  (if (null? (cdr list1))
      (if (pair? (car list1))
          (list (deep-reverse (car list1)))
          list1)
      (cons (if (pair? (last-pair list1))
                (deep-reverse (last-pair list1))
                (last-pair list1))
            (deep-reverse (no-last list1)))))

;Exercise 2.28

(define (fringe list1)
  (if (null? list1)
      null
      (append (if (pair? (car list1))
                  (fringe (car list1))
                  (list (car list1)))
              (fringe (cdr list1)))))

;Exercise 2.29

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

;a

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

;b

(define (total-weight mobile)
  (let ((ls (branch-structure (left-branch mobile)))
        (ll (branch-length (left-branch mobile)))
        (rs (branch-structure (right-branch mobile)))
        (rl (branch-length (right-branch mobile))))
    (+ (if (= 1 ll)
           ls
           (total-weight ls))
       (if (= 1 rl)
           rs
           (total-weight rs)))))
     
(define l4 (make-branch 1 1))
(define r4 (make-branch 1 2))
(define bi4 (make-mobile l4 r4))
(define l3 (make-branch 1 4))
(define r3 (make-branch 1 5))
(define l2 (make-branch 2 bi4))
(define r2 (make-branch 1 3))
(define bi2 (make-mobile l2 r2))
(define bi3 (make-mobile l3 r3))
(define l1 (make-branch 3 bi2))
(define r1 (make-branch 2 bi3))
(define bi1 (make-mobile l1 r1))

(define bi5 (make-mobile l2 l2))

;c

(define (balanced-submobile? mobile)
  (let ((ll (branch-length (left-branch mobile)))
        (lw (if (number? (branch-structure (left-branch mobile)))
                (branch-structure (left-branch mobile))
                (total-weight (branch-structure (left-branch mobile)))))
        (rl (branch-length (right-branch mobile)))
        (rw (if (number? (branch-structure (right-branch mobile)))
                (branch-structure (right-branch mobile))
                (total-weight (branch-structure (right-branch mobile))))))
    (if (= (* ll lw) (* rl rw))
        true
        false)))
    
(define (make-list-mobile mobile)
  (let ((ls (branch-structure (left-branch mobile)))
        (rs (branch-structure (right-branch mobile))))
    (cons mobile
          (append (if (number? ls)
                      null
                      (make-list-mobile ls))
                  (if (number? rs)
                      null
                      (make-list-mobile rs))))))

(define (balanced? mobile)
  (let ((lm (make-list-mobile mobile)))
    (define (iter lm)
      (cond ((null? (cdr lm)) "balanced")
            ((balanced-submobile? (car lm)) (iter (cdr lm)))
            ("not balanced")))
  (iter lm)))

;mapping over trees

(define (scale-tree tree factor)
  (cond ((null? tree) null)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree-map tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-map sub-tree factor)
             (* sub-tree factor)))
       tree))

;Exercise 2.30

(define (square-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
       tree))

;Exercise 2.31

(define (tree-map proc tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))
(define (square-tree-new tree)
  (tree-map square tree))

;Exercise 2.32

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (append (list (car s)) x))
                          rest)))))

;sequences as conventional interfaces

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        null
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

;sequence operations

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares-new tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs-new n)
  (accumulate
   cons
   null
   (filter even? (map fib (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate
   cons
   null
   (map square (map fib (enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

;Exercise 2.33

(define (map-new p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(define (append-new seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-new sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;Exercise 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

;Exercise 2.35

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (count-leaves-new t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

;Exercise 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

;Exercise 2.37

;cp1

;Exercise 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;Exercise 2.39

(define (reverse-right sequence)
  (accumulate (lambda (x y) (append y (list x))) null sequence))

(define (reverse-left sequence)
  (fold-left (lambda (x y) (append (list y) x)) null sequence))

;nested mapping

(define (proba n)
  (accumulate
   append null (map (lambda (i)
                      (map (lambda (j) (list i j))
                           (enumerate-interval 1 (- i 1))))
                    (enumerate-interval 1 n))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (prime? n)
        (= n (smallest-divisor n)))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                           (lambda (i)
                             (map (lambda (j) (list i j))
                                  (enumerate-interval 1 (- i 1))))
                           (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)
      (list null)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;Exercise 2.40

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs-new n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;Exercise 2.41

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (sum-less-or-equal? s)
  (lambda (triple-list)
    (not (> (+ (car triple-list)
               (cadr triple-list)
               (car (cdr (cdr triple-list))))
            s))))

(define (make-triple-sum triple-list)
  (list (car triple-list)
        (cadr triple-list)
        (car (cdr (cdr triple-list)))
        "sum"
        (+ (car triple-list)
           (cadr triple-list)
           (car (cdr (cdr triple-list))))))

(define (find-triples-less-or-equal n s)
  (map make-triple-sum
       (filter (sum-less-or-equal? s)
               (unique-triples n))))

;Exercise 2.42

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (list k new-row))))

(define empty-board
  null)

 (define (safe? k positions)
   (if (safe?1 k positions)
       (safe?2 k positions)
       false))

(define (safe?1 k positions)
  (or (= k 1)
      (let ((p (last-pair positions))
            (pl (last-pair (no-last positions))))
        (> (abs (- (cadr p) (cadr pl))) 1))))
 
(define (safe?2 k positions)
  (let ((p (last-pair positions)))
    (define (iter k list)
      (if (> k 1)
          (let ((pl (last-pair (no-last list))))
            (if (not (= (cadr p) (cadr pl)))
                (iter (- k 1) (no-last list))
                false))
          true))
    (iter k positions)))

;Exercise 2.43

(define (queens-slow board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

;symbolic data
;quotation

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;Exercise 2.54

(define (equal-list? list1 list2)
  (cond ((not (= (length list1) (length list2))) false)
        ((and (null? (cdr list1)) (null? (cdr list2))) true)
        ((and (pair? (car list1)) (pair? (car list2))) (equal? (car list1) (car list2)))
        ((eq? (car list1) (car list2)) (equal? (cdr list1) (cdr list2)))
        (else false)))

;symbolic differentiation

;(variable? e) is e a variable?
;(same-variable? v1 v2) are v1 and v2 the same variable?
;(sum? e) is e a sum?
;(addend e) addend of the sum e
;(augend e) augend of the sum e
;(make-sum a1 a2) construct the sum of a1 and a2
;(product? e) is e a product?
;(multiplier e) multiplier of the product e
;(multiplicand e) multiplicand of the product e
;(make-product m1 m2) construct the product of m1 and m2

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-exponentiation (base exp)
                                            (- (exponent exp) 1))))
        (else
         (error "unknown exprission type: DERIV" exp))))

;(define (variable? x) (symbol? x))
;(define (same-variable? v1 v2)
;  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;(define (make-sum a1 a2)
;  (cond ((=number? a1 0) a2)
;        ((=number? a2 0) a1)
;        ((and (number? a1) (number? a2))
;         (+ a1 a2))
;        (else (list '+ a1 a2))))

;(define (=number? exp num) (and (number? exp) (= exp num)))

;(define (make-product m1 m2)
;  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;        ((=number? m2 1) m1)
;        ((and (number? m1) (number? m2)) (* m1 m2))
;        (else (list '* m1 m2))))

;(define (sum? x) (and (pair? x) (eq? (car x) '+)))
;(define (addend s) (cadr s))
;(define (augend s)
;  (if (> (length s) 3)
;      (without-second s)
;      (caddr s)))

;(define (product? x) (and (pair? x) (eq? (car x) '*)))
;(define (multiplier p) (cadr p))
;(define (multiplicand p)
;  (if (> (length p) 3)
;      (without-second p)
;      (caddr p)))

;Exercise 2.56

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (make-exponentiation b e)
  (cond ((= e 0) 1)
        ((= e 1) b)
        (else (list '** b e))))

;Exercise 2.57

(define (without-second list1)
  (define (iter list3)
            (cons (car list3)
                  (if (null? (cdr list3))
                      null
                      (iter (cdr list3)))))
  (let ((list2 (cdr (cdr list1))))
    (cons (car list1)
          (iter list2))))

;Exercise 2.58

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x) (and (pair? x) (is-there-+? x)))
(define (is-there-+? x)
  (cond ((null? x) false)
        ((eq? '+ (car x)) true)
        (else (is-there-+? (cdr x)))))
(define (addend s)
  (define (iter s)
    (cons (car s)
          (if (eq? '+ (car (cdr s)))
              null
              (iter (cdr s)))))
  (if (null? (cdr (iter s)))
             (car (iter s))
             (iter s)))
(define (augend s)
  (define (iter s)
    (if (eq? '+ (car (cdr s)))
        (cdr (cdr s))
        (augend (cdr s))))
  (if (null? (cdr (iter s)))
      (car (iter s))
      (iter s)))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

;exemple: representing sets

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;Exercise 2.59

(define (union-set-old set1 set2)
  (define (iter result set1)
    (cond ((null? set1) result)
          ((element-of-set? (last-pair set1) set2)
           (iter result (no-last set1)))
          (else (iter (cons (last-pair set1) result) (no-last set1)))))
  (iter set2 set1))
  
(define (union-set set1 set2)
  (define (iter set1 result)
    (if (null? set1)
        result
        (iter (no-last set1)
              (adjoin-set (last-pair set1) result))))
    (iter set1 set2))
  
;Exercise 2.60

(define (no-duplicates set)
  (define (iter set result)
    (if (null? set)
        result
        (iter (cdr set)
              (adjoin-set (car set)
                          result))))
  (reverse (iter set null)))

(define (element-of-set-nd? x set)
  (let ((set (no-duplicates set)))
    (element-of-set? x set)))

(define (adjoin-set-nd x set)
  (let ((set (no-duplicates set)))
    (adjoin-set x set)))

(define (union-set-nd set1 set2)
  (let ((set1 (no-duplicates set1))
        (set2 (no-duplicates set2)))
    (union-set set1 set2)))

(define (intersection-set-nd set1 set2)
  (let ((set1 (no-duplicates set1))
        (set2 (no-duplicates set2)))
    (intersection-set set1 set2)))


;sets as ordered lists

(define (element-of-set-ol? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set-ol? x (cdr set)))))

(define (intersection-set-ol set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-ol (cdr set1)
                                             (cdr set2))))
              ((< x1 x2)
               (intersection-set-ol (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-ol set1 (cdr set2)))))))

;Exercise 2.61

(define (adjoin-set-ol-old x list1)
  (fringe
   (cond ((element-of-set-ol? x list1) list1)
         ((> x (last-pair list1)) (list list1 x))
         ((< x (car list1)) (list x list1))
         (else (cons (car list1)
                     (adjoin-set-ol x (cdr list1)))))))

(define (adjoin-set-ol-iter x list1)
  (define (iter result set)
    (cond ((> x (last-pair list1)) (list list1 x))
          ((element-of-set-ol? x (fringe result)) (cons (reverse result) set))
          (else (iter (cons (if (< x (car set))
                                (list x (car set))
                                (car set))
                            result)
                      (cdr set)))))
  (fringe (iter '() list1)))

(define (adjoin-set-ol x set)
  (cond ((element-of-set-ol? x set) set)
        ((null? set) (list x))
        (else
         (cond ((= x (car set)) set)
               ((< x (car set)) (cons x
                                      set))
               ((> x (car set)) (cons (car set)
                                      (adjoin-set-ol x (cdr set))))))))
 
;Exercise 2.62

(define (union-set-ol set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2) (cons x1
                                  (union-set-ol (cdr set1)
                                                (cdr set2))))
                 ((< x1 x2) (cons x1
                                  (union-set-ol (cdr set1)
                                                set2)))
                 ((> x1 x2) (cons x2
                                  (union-set-ol set1
                                                (cdr set2)))))))))

;sets as binary trees

(define (entry tree) (car tree))
(define (left-branch-bt tree) (cadr tree))
(define (right-branch-bt tree) (caddr tree))
(define (make-tree-bt entry left right)
  (list entry left right))

(define (element-of-set-bt? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set-bt? x (left-branch-bt set)))
        ((> x (entry set))
         (element-of-set-bt? x (right-branch-bt set)))))

(define (adjoin-set-bt x set)
  (cond ((null? set) (make-tree-bt x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree-bt (entry set)
                       (adjoin-set-bt x (left-branch-bt set))
                       (right-branch-bt set)))
        ((> x (entry set))
         (make-tree-bt (entry set)
                       (left-branch-bt set)
                       (adjoin-set-bt x (right-branch-bt set))))))

(define set-5
  (adjoin-set-bt 11 (adjoin-set-bt 7 (adjoin-set-bt 1 (adjoin-set-bt 3 (adjoin-set-bt 9 (adjoin-set-bt 5 null)))))))

;Exercise 2.63

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch-bt tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch-bt tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch-bt tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch-bt tree)
                                          result-list)))))
  (copy-to-list tree '()))
      
;Exercise 2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree-bt this-entry
                                    left-tree
                                    right-tree)
                      remaining-elts))))))))

;Exercise 2.66

;(define (lookup given-key set-of-records)
;  (cond ((null? set-of-records) false)
;        ((= given-key (key (entry set-of-records))) true)
;        ((< given-key (key (entry set-of-records)))
;         (lookup given-key (left-branch set-of-records)))
;        ((> given-key (key (entry set-of-records)))
;         (lookup given-key (right-branch set-of-records)))))

;representing Huffman trees

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch-huf tree) (car tree))
(define (right-branch-huf tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define leaf-A (make-leaf 'A 8))
(define leaf-B (make-leaf 'B 3))
(define leaf-C (make-leaf 'C 1))
(define leaf-D (make-leaf 'D 1))
(define leaf-E (make-leaf 'E 1))
(define leaf-F (make-leaf 'F 1))   
(define leaf-G (make-leaf 'G 1))  
(define leaf-H (make-leaf 'H 1))        
     
(define tree-CD (make-code-tree leaf-C leaf-D))
(define tree-EF (make-code-tree leaf-E leaf-F))
(define tree-GH (make-code-tree leaf-G leaf-H))
(define tree-EFGH (make-code-tree tree-EF tree-GH))
(define tree-BCD (make-code-tree leaf-B tree-CD))
(define tree-BCDEFGH (make-code-tree tree-BCD tree-EFGH))
(define tree-ABCDEFGH (make-code-tree leaf-A tree-BCDEFGH))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set-huf x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set-huf x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set-huf (make-leaf (car pair)
                                   (cadr pair))
                        (make-leaf-set (cdr pairs))))))

;Exercise 2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;Exercise 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-set? symbol (symbols (left-branch-huf tree)))
         (cons 0
               (encode-symbol symbol (left-branch-huf tree))))
        ((element-of-set? symbol (symbols (right-branch-huf tree)))
         (cons 1
               (encode-symbol symbol (right-branch-huf tree))))
        (else (error "the symbol is not it the tree" tree))))

;Exercise 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leafs)
  (define (iter result leafs)
    (if (null? leafs)
        result
        (iter (if (> (weight result) (* 2 (weight (car leafs))))
                  (make-code-tree (make-code-tree (car leafs)
                                                  (left-branch-huf result))
                                  (right-branch-huf result))
                  (make-code-tree (car leafs)
                                  result))
              (cdr leafs))))
  (iter (car leafs) (cdr leafs)))

;incorrect solution
(define (successive-merge-old leafs)
  (define (successive-merge-1 leafs)
    (if (null? (cdr leafs))
        (car leafs)
        (make-code-tree (car leafs)
                        (successive-merge-1 (cdr leafs)))))
  (successive-merge-1 (reverse leafs)))

;Exercise 2.70

(define rock-tree
  (generate-huffman-tree
   '((NA 16) (YIP 9) (SHA 3) (JOB 2) (GET 2) (A 2) (WAH 1) (BOOM 1))))
  
(define encode-song
  (encode '(GET A JOB
            SHA NA NA NA NA NA NA NA NA
            GET A JOB
            SHA NA NA NA NA NA NA NA NA
            WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
            SHA BOOM)
          rock-tree))

;Exercise 2.71

(define (make-seq-2 n)
  (define (iter m n)
    (if (= n 0)
        null
        (cons (power 2 m)
              (iter (- m 1) (- n 1)))))
  (iter (- n 1) n))
(define (sum-seq seq)
  (if (null? seq)
      0
      (+ (car seq)
         (sum-seq (cdr seq)))))

;tagged data
  
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: TYPE-TAG" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum: CONTENTS" datum))))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

;rectangular form

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z) (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

;polar form

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

;(define (real-part z)
;  (cond ((rectangular? z)
;         (real-part-rectangular (contents z)))
;        ((polar? z)
;         (real-part-polar (contents z)))
;        (else (error "Unknown type: REAL-PART" z))))
;(define (imag-part z)
;  (cond ((rectangular? z)
;         (imag-part-rectangular (contents z)))
;        ((polar? z)
;         (imag-part-polar (contents z)))
;        (else (error "Unknown type: IMAG-PART" z))))
;(define (magnitude z)
;  (cond ((rectangular? z)
;         (magnitude-rectangular (contents z)))
;        ((polar? z)
;         (magnitude-polar (contents z)))
;        (else (error "Unknown type: MAGNITUDE" z))))
;(define (angle z)
;  (cond ((rectangular? z)
;         (angle-rectangular (contents z)))
;         (angle-polar (contents z)))
;        (else (error "Unknown type: ANGLE" z))))
;(define (make-from-real-imag x y)
;  (make-from-real-imag-rectangular x y))
;(define (make-from-mag-ang r a)
;  (make-from-mag-ang-polar r a))

;data-directed programming and additivity

;put and get implementations
(define global-array '())
(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))
(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))
(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))
;done

;representations for complex numbers

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(define (install-rectangular-package)
  ;;internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;;interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;;internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (sub (magnitude z) (cose (angle z))))   ;sub cos
  (define (imag-part z) (sub (magnitude z) (sine (angle z))))   ;sub sin
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))  
  ;;interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic-simple op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;(define number1 (list 'rectangular (list 1 2)))
;(define number2 (list 'rectangular (list 3 4)))

;Exercise 2.73

(define (deriv-dd exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv-dd (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;b

(define (big-deriv-package)
  (deriv-sum-package)
  (deriv-product-package)
  (deriv-exponentiation-package))
  
(define (deriv-sum-package)
  (define (deriv-sum exp var)
    (make-sum (deriv-dd (addend exp) var)
              (deriv-dd (augend exp) var)))
  (put 'deriv-dd '+ deriv-sum)
  (define (addend s) (car s))
  (define (augend s)
    (if (> (length s) 2)
        (cons '+ (cdr s))
        (cadr s)))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list '+ a1 a2))))
  (put 'make '+ make-sum)
  'done)

(define (deriv-product-package)
  (define (deriv-product exp var)
    ((get 'make '+)
          (make-product (multiplier exp)
                        (deriv-dd (multiplicand exp) var))
          (make-product (deriv-dd (multiplier exp) var)
                        (multiplicand exp))))
    (put 'deriv-dd '* deriv-product)
    (define (multiplier s) (car s))
    (define (multiplicand s)
      (if (> (length s) 2)
          (cons '* (cdr s))
          (cadr s)))
    (define (make-product m1 m2)
      (cond ((or (=number? m1 0) (=number? m2 0)) 0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (number? m1) (number? m2)) (* m1 m2))
            (else (list '* m1 m2))))
  (put 'make '* make-product)
  'done)

;c
 
(define (deriv-exponentiation-package)
  (define (deriv-exponentiation exp var)
    ((get 'make '*) (exponent exp)
                    (make-exponentiation (base exp)
                                         (- (exponent exp) 1))))
  (put 'deriv-dd '** deriv-exponentiation)
  (define (base e) (car e))
  (define (exponent e) (cadr e))
  (define (make-exponentiation b e)
    (cond ((= e 0) 1)
          ((= e 1) b)
          (else (list '** b e))))
  'done)

;Exercise 2.74

;a
  
(define (div1-package)
  (define (name r) (car r))
  (define (adress r) (cadr r))
  (define (salary r) (caddr r))
  (define (get-record name1 file)
    (cond ((null? file) null)
          ((eq? (name (car file)) name1) (car file))
          (else (get-record name1 (cdr file)))))
  (define (get-salary name1 file)
    (if (null? (get-record name1 file))
        null
        (salary (get-record name1 file))))
  (put 'get-salary 'div1 get-salary)
  (put 'get-record 'div1 get-record)
  'done)
(define (div2-package)
  (define (name r) (car r))
  (define (adress r) (car (cadr r)))
  (define (salary r) (cadr (cadr r)))
  (define (group1 f) (car f))
  (define (group2 f) (cadr f))
  (define (get-record name1 file)
    (let ((file1 (list (car (group1 file))
                       (car (group2 file))
                       (cadr (group2 file)))))
      (define (iter name1 file)
        (cond ((null? file) null)
              ((eq? (name (car file)) name1) (car file))
              (else (iter name1 (cdr file)))))
      (iter name1 file1)))
  (define (get-salary name1 file)
    (if (null? (get-record name1 file))
        null
        (salary (get-record name1 file))))
  (put 'get-salary 'div2 get-salary)
  (put 'get-record 'div2 get-record)
  'done)
  
(define record-chandler (list 'chandler 'new-york 10000))
(define record-ross (list 'ross 'new-york 9000))
(define record-joey (list 'joey 'new-york 5000))
(define div1-file (attach-tag 'div1 (list record-chandler
                                          record-ross
                                          record-joey)))
(define record-monica (list 'monica (list 'new-york '9000)))
(define record-phoebe (list 'phoebe (list 'new-york '5000)))
(define record-rachel (list 'rachel (list 'new-york '5000)))
(define div2-file (attach-tag 'div2 (list (list record-phoebe)
                                          (list record-monica
                                                record-rachel))))
;a

(define (get-record name file)
  ((get 'get-record (type-tag file)) name (contents file)))

;b

(define (get-salary name)
  (lambda (file)
    ((get 'get-salary (type-tag file)) name (contents file))))

(define (get-salary-common name . file)
  (map (get-salary name) file))
     
(define (get-salary-common1 name . file)
  (simple-map (get-salary name) file))

(define (get-salary1 name . files)
  (cond ((null? files) (error "isn't this record here"))
        ((not (null? (get-record name (car files))))
        ((get 'get-salary (type-tag (car files))) name (contents (car files))))
        (else (get-salary1 name (cdr files)))))

;c

(define (get-record1 name)
  (lambda (file)
    ((get 'get-record (type-tag file)) name (contents file))))

(define (find-employee-record1 name . files)
  (map (get-record1 name) files))
           
(define (simple-map proc items)
  (cond ((null? items) null)
        ((null? (proc (car items))) (simple-map proc (cdr items)))
        (else (proc (car items)))))
  
(define (find-employee-record2 name . files)
  (simple-map (get-record1 name) files))   

;message passing

(define (make-from-real-imag-mp x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part-mp) x)
          ((eq? op 'imag-part-mp) y)
          ((eq? op 'magnitude-mp) (sqrt (+ (square x) (square y))))
          ((eq? op 'angle-mp) (atan y x))
          (else (error "Unknown op: MAKE-FROM-REAL-IMAG-MP" op))))
  dispatch)

(define (apply-generic-mp op arg) (arg op))

;Exercise 2.75

(define (make-from-mag-ang-mp r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part-mp) (* r (cos a)))
          ((eq? op 'imag-part-mp) (* r (sin a)))
          ((eq? op 'magnitude-mp) r)
          ((eq? op 'angle-mp) a)
          (else (error "Unknown op: MAKE-FROM-MAG-ANG-MP" op))))
  dispatch)
       
(define (real-part-mp z)
  (apply-generic-mp 'real-part-mp z))
(define (imag-part-mp z)
  (apply-generic-mp 'imag-part-mp z))
(define (magnitude-mp z)
  (apply-generic-mp 'magnitude-mp z))
(define (angle-mp z)
  (apply-generic-mp 'magnitude-mp z))

(define (add-complex-mp z1 z2)
  (cons (+ (real-part-mp z1) (real-part-mp z2))
        (+ (imag-part-mp z1) (imag-part-mp z2))))

;(define number1 (make-from-real-imag-mp 1 2))
;(define number2 (make-from-real-imag-mp 3 4))
;(define number3 (make-from-mag-ang-mp 5 6))
;(define number4 (make-from-mag-ang-mp 7 8))

;generic arithmitic operations

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (greatest-common-divisor x y) (apply-generic 'gcd x y))
(define (reduce x y) (apply-generic 'reduce x y))

(define (install-scheme-number-package)
  (define (equ? x y)
    (if (= x y)
        true
        false))
  (define (=zero? x)
    (if (= x 0)
        true
        false))
  (define (scheme-gcd x y) (gcd x y))
  (define (tag x) (attach-tag 'scheme-number x))
  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (equ? x y)))
  (put '=zero? '(scheme-number) =zero?)
  (put 'gcd '(scheme-number scheme-number)
       (lambda (x y) (tag (scheme-gcd x y))))
  (put 'reduce '(scheme-number scheme-number)
       (lambda (x y) (tag (reduce-integers x y))))
  ;;following added to Scheme-number package
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (gcd-terms a b) (apply-generic 'gcd-terms a b))

(define (install-rational-package)
  ;;internal procedures
  (define (numer x) (car x))
  (define (denom x) (cadr x))
;  (define (make-rat n d)
;    (let ((g (greatest-common-divisor n d)))
;      (list (div n g) (div d g))))
  
  (define (make-rat n d) (reduce n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (equ? x y)
    (if (and (= (numer x) (numer y))
             (= (denom x) (denom y)))
        true
        false))
  (define (=zero? x)
    (if (= (numer x) 0)
        true
        false))
  (define (sine r)                        ;sine
    (make-rat (sin (numer r))
              (sin (denom r))))
  (define (cose r)                        ;cose
    (make-rat (cos (numer r))
              (cos (denom r))))
  ;;interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ? x y)))
  (put '=zero? '(rational)
       (lambda (x) (=zero? x)))
  (put 'cose '(rational)                    ;cose
       (lambda (x) (tag (cose x))))      
  (put 'sine '(rational)                    ;sine
       (lambda (x) (tag (sine x))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;;imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;;internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))      ;add
                         (add (imag-part z1) (imag-part z2))))    ;add
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ? z1 z2)
    (if (and (= (real-part z1) (real-part z2))
             (= (imag-part z1) (imag-part z2)))
        true
        false))
  (define (=zero? x)
    (if (and (or (> (magnitude x) 0)
                 (= (magnitude x) 0))
             (< (magnitude x) 0.001))
        true
        false))
  ;;interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (equ? z1 z2)))
  (put '=zero? '(complex)
       (lambda (x) (=zero? x)))
  ;;Exercise 2.77
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (run-all-number-packages)
  (install-rectangular-package)
  (install-polar-package)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package))

;put-coercion and get-coercion implementations
(define global-array-coercion '())
(define (put-coercion op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array-coercion (put-helper (list op type) global-array-coercion)))
(define (get-coercion op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array-coercion))
;done
  
;combining data of different types
;coercion

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number
              'complex
              scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "Stop coercion")
                    (let ((t1->t2 (get-coercion type1 type2))
                         (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else (error "No method for these types"
                                         (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;Exercise 2.82

(define (apply-generic-generalize op . args)
  (define (iter arguments)
    (if (null? arguments)
        (error "no method")
        (let ((coercion (coercion-to-list (car arguments) args)))
          (if (= (length coercion) (length args))
              (let ((proc (get op (list (type-tag (car arguments)) (type-tag (car arguments))))))
                (apply-to-list proc coercion))
              (iter (cdr arguments))))))
  (iter args))

(define (coercion-to-list arg args)
  (if (null? args)
      null
      (let ((proc (get-coercion (type-tag (car args)) (type-tag arg))))
        (if proc
            (cons (proc (car args))
                  (coercion-to-list arg (cdr args)))
            null))))

(define (apply-to-list proc list1)
  (define (iter result list1)
    (if (null? list1)
        result
        (iter (proc result (car list1)) (cdr list1))))
  (iter (car list1) (cdr list1)))

(define (four-add a b c d) (apply-generic-generalize 'add a b c d))

;Exercise 2.81

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number
              'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

;a

(define (exp x y) (apply-generic 'exp x y))
                        
;Exercise 2.83

(define (raise-integer->rational i)
  (make-rational i 1))
(put-coercion 'raise
              'scheme-number
              raise-integer->rational)
(define (raise-rational->real r)
  (cons 'real (/ (numer (contents r)) (denom (contents r)))))
(put-coercion 'raise
              'rational
              raise-rational->real)
(define (raise-real->complex r)
  (make-complex-from-real-imag (contents r) 0))
(put-coercion 'raise
              'real
              raise-real->complex)

(define (raise number)
  ((get-coercion 'raise (type-tag number)) number))
  
;Exercise 2.84

(define (apply-generic-raise op . args)
  (let ((arg1 (car args))
        (arg2 (cadr args)))
    (define (iter arg1 arg2)
      (let ((type1 (numbers-extractor arg1))
            (type2 (numbers-extractor arg2)))
        (cond ((= type1 type2) (drop (apply-generic op arg1 arg2)))
              ((> type1 type2) (iter arg1 (raise arg2)))
              (else (iter (raise arg1) arg2)))))
    (iter arg1 arg2)))
  

(define numbers-ierarhical-list
  (list (attach-tag 1 'scheme-number)
        (attach-tag 2 'rational)
        (attach-tag 3 'real)
        (attach-tag 4 'complex)))
      
(define (numbers-extractor n)
  (define (iter list1)
    (cond ((null? list1) (error "No number in the list1"))
          ((eq? (type-tag n) (contents (car list1))) (type-tag (car list1)))
          (else (iter (cdr list1)))))
  (iter numbers-ierarhical-list))
  
(define (add-raise x y) (apply-generic-raise 'add x y))

;Exercise 2.85

;(run-all-number-packages)

(define (project-complex->real c)
  (if (= (imag-part (contents c)) 0)
      (cons 'real
            (real-part (contents c)))
      c))
(put-coercion 'project
              'complex
              project-complex->real)
(define (project-real->integer r)
  (let ((num (contents r)))
    (if (= num (round num))
        (make-scheme-number num)
        r)))
(put-coercion 'project
              'real
              project-real->integer)
(define (project-rational->integer r)
  (let ((num (/ (numer (contents r)) (denom (contents r)))))
    (if (= num (round num))
        (make-scheme-number num)
        r)))
(put-coercion 'project
              'rational
              project-rational->integer)

(define (project number)
  (let ((p-n (get-coercion 'project (type-tag number))))
    (if p-n
        (p-n number)
        number)))

(define (drop number)
  (let ((p-n (project number)))
    (if (eq? (type-tag p-n) (type-tag number))
        number
        (drop p-n))))
        
;Exercise 2.86

(define (sine x) (apply-generic 'sine x))
(define (cose x) (apply-generic 'cose x))

;Example: symbolic algebra

(define (install-sparse-polynomial-package)
  ;;internal procedures
  ;;representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;;representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (empty-termlist? term-list) (null? term-list))
  (define (first-term term-list) (car term-list))
  (define (order term) (car term))
  (define (rest-terms term-list) (cdr term-list))
  (define (make-term order coeff) (list order coeff))
 
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (the-empty-termlist) '())
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (coeff term) (cadr term))
  (define (=zero? term)
    (if (pair? term)
        (if (null? (term-list term))
            true
            false)
        (=zero-numer? term)))
  (define (=zero-numer? x)
    (if (= x 0)
        true
        false))
  
  (define (neg-terms L)
    (if (empty-termlist? L)
        L
        (let ((neg (* -1 (coeff (first-term L)))))
          (let ((neg-term (make-term (order (first-term L)) neg)))
            (adjoin-term neg-term
                         (neg-terms (rest-terms L)))))))
  (define (sub-terms L1 L2)
    (add-terms L1 (neg-terms L2)))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (div-terms (sub-terms L1 (mul-terms (list (make-term new-o new-c)) L2)) L2)))
                  (adjoin-term (make-term new-o new-c) rest-of-result)))))))

  (define (another-term-poly L)
    (define (iter x counter)
      (if (null? x)
          (error "Ooops")
          (if (pair? (coeff (first-term x)))
              (exchange-terms L counter)
              (iter (rest-terms x) (+ 1 counter)))))
    (iter L 1))

  (define (raw-term-list L)
    (define (iter result list1)
      (if (empty-termlist? list1)
          result
          (iter (append result
                        (list (coeff (first-term list1))))
                (rest-terms list1))))
    (iter (the-empty-termlist) L))
        
  (define (gcd-terms a b)
    (let ((r-t (pseudoremainder-terms a b)))
      (let ((r-t-l (raw-term-list r-t)))
        (let ((r-d (apply gcd r-t-l)))
          (let ((n-r-t (list (make-term 0 (/ 1 r-d)))))
            (mul-terms r-t n-r-t))))))
;    (if (empty-termlist? b)
;        a
;        (gcd-terms b (remainder-terms a b))))
  (define (remainder-terms a b)
    (last-pair (div-terms a b)))
  (define (pseudoremainder-terms p q)
    (let ((o1 (order (first-term (term-list p))))
          (o2 (order (first-term (term-list q))))
          (c (coeff (first-term (term-list q)))))
      (let ((integerizing-factor
             (list (make-term 0
                              (power c (+ 1 (- o1 o2)))))))
        (last-pair
        (div-terms (mul-terms p integerizing-factor)
                   q)))))

  (define (reduce-terms n d)
    (let ((g-t (gcd-terms n d)))
      (let ((c (coeff (first-term (term-list g-t))))
            (o2 (order (first-term (term-list g-t))))
            (o1 (max-of-orders n d)))
        (let ((integerizing-factor
               (list (make-term 0
                                (power c (+ 1 (- o1 o2)))))))
          (let ((nn (mul-terms (no-last (no-last (div-terms n g-t))) integerizing-factor))
                (dd (mul-terms (no-last (no-last (div-terms d g-t))) integerizing-factor)))
            (list nn dd))))))
  (define (max-of-orders t1 t2)
    (let ((o1 (order (first-term (term-list t1))))
          (o2 (order (first-term (term-list t2)))))
      (if (> o1 o2)
          o1
          o2)))

  ;ex. 2.92 
  (define (exchange-terms L n)
    (let ((goal-pair (pick-pair L n))
          (else-pairs (without-picked L n)))
      (list (cons 'sparse else-pairs)
            (list 'sparse goal-pair))))
    
;  ;;interface to rest of the system
  (define (tag p) (attach-tag 'sparse p))
  (put 'term-list '(sparse)
       (lambda (p) (tag (term-list p))))
  (put 'add-terms '(sparse sparse)
       (lambda (x y) (tag (add-terms x y))))
  (put 'mul-terms '(sparse sparse)
       (lambda (x y) (tag (mul-terms x y))))
  (put 'neg-terms '(sparse)
       (lambda (x) (tag (neg-terms x))))
  (put 'div-terms '(sparse sparse)
       (lambda (x y) (tag (div-terms x y))))
  (put 'another-term-poly '(sparse) another-term-poly)
  (put 'gcd-terms '(sparse sparse)
       (lambda (x y) (tag (gcd-terms x y))))
  (put 'reduce-terms '(sparse sparse)
       (lambda (x y) (list (tag (car (reduce-terms x y)))
                           (tag (cadr (reduce-terms x y))))))
  'done)


;Exercise 2.88

(define (negative p) (apply-generic 'neg p))

;Exercise 2.89

(define (install-dense-polynomial-package)
  ;;internal procedures
  ;;representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;;representation of terms and term lists
;  (define (adjoin-term term term-list)
;    (if (null? term)
;        term-list
;        (cons term term-list)))
    (define (adjoin-term term term-list)
    (if (=zero? term)
        term-list
        (cons term term-list)))
  (define (=zero? term)
    (if (pair? term)
        (if (null? (term-list term))
            true
            false)
        (=zero-numer? term)))
  (define (=zero-numer? x)
    (if (= x 0)
        true
        false))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order L1) (order L2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order L1) (order L2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term t1 t2)
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (make-term t1 t2) (add t1 t2))  
  (define (empty-termlist? term-list) (null? term-list))
  (define (first-term term-list) (car term-list))
  (define (order term-list)
    (define (iter counter list1)
      (if (null? list1)
          counter
          (iter (+ 1 counter) (cdr list1))))
    (iter 0 term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (raise-order (mul-term-by-all-terms (first-term L1) L2) L1)
                   (mul-terms (rest-terms L1) L2))))
  (define (raise-order ml l)
    (define (iter result list)
      (if (null? (cdr list))
          result
          (iter (reverse (cons 0 (reverse result))) (cdr list))))
    (iter ml l))
  (define (the-empty-termlist) '())
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (mul t1 t2)
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (neg-terms L)
    (if (empty-termlist? L)
        L
        (let ((neg-term (* -1 (first-term L))))
          (adjoin-term neg-term
                       (neg-terms (rest-terms L))))))
  ;;interface to rest of the system
  (define (tag p) (attach-tag 'dense p))
  (put 'add-terms '(dense dense)
       (lambda (x y) (tag (add-terms x y))))
  (put 'neg-terms '(dense)
       (lambda (x) (tag (neg-terms x))))
  (put 'mul-terms '(dense dense)
       (lambda (x y) (tag (mul-terms x y))))
  'done)

(define (variable p) (apply-generic 'variable p))
(define (term-list p) (apply-generic 'term-list p))
(define (add-terms  L1 L2) (apply-generic 'add-terms L1 L2))
(define (neg-terms l) (apply-generic 'neg-terms l))
(define (mul-terms L1 L2) (apply-generic 'mul-terms L1 L2))
(define (div-terms L1 L2) (apply-generic 'div-terms L1 L2))
(define (another-term-poly x) (apply-generic 'another-term-poly x))
(define (reduce-terms  L1 L2) (apply-generic 'reduce-terms L1 L2))

(define (install-polynomial-package)
  ;imported procedures from sparse and dense packages
  (define (make-poly variable term-list)(cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (let ((ap (another-term-poly (term-list p2))))
          (if ap
              (let ((sp1 (make-poly (variable p2) (car ap)))
                    (sp2 (make-poly (variable p2) (cadr ap))))
                (display sp1)
                (newline)
                (display sp2))
              (error "Polys not in same var: MUL-POLY")))))
  (define (sub-poly p1 p2)
    (add-poly p1
              (neg-poly p2)))
  (define (neg-poly p)
    (make-poly (variable p)
               (neg-terms (term-list p))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((new-terms-list (div-terms (term-list p1) (term-list p2))))
          (make-poly (variable p1) new-terms-list))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (gcd-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((r-t (reduce-terms (term-list p1) (term-list p2))))
          (list (make-poly (variable p1) (car r-t))
                (make-poly (variable p1) (cadr r-t))))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
        
  ;interface to the rest of the system`
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'gcd '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  (put 'make-poly 'polynomial make-poly)
  (put 'variable '(polynomial) variable)
  (put 'term-list '(polynomial) term-list)
  (put 'reduce '(polynomial polynomial)
       (lambda (p1 p2) (list (tag (car (reduce-poly p1 p2)))
                             (tag (cadr (reduce-poly p1 p2))))))
  
  'done)

(install-sparse-polynomial-package)
(install-dense-polynomial-package)
(install-polynomial-package)
(run-all-number-packages)

(define (make-dense-polynomial var terms)
  (attach-tag 'polynomial
              ((get 'make-poly 'polynomial) var (cons 'dense terms))))
(define poly3 (make-dense-polynomial 'x '(1 2 3 0 4 5)))
(define poly4 (make-dense-polynomial 'x '(5 0 0 5)))
(define poly5 (make-dense-polynomial 'x '(1 0 2)))

(define (make-sparse-polynomial var terms)
  (attach-tag 'polynomial
              ((get 'make-poly 'polynomial) var (cons 'sparse terms))))
(define poly6 (make-sparse-polynomial 'x '((5 1) (0 -1))))
(define poly7 (make-sparse-polynomial 'x '((2 1) (0 -1))))
(define poly1 (make-sparse-polynomial 'x (list (list 100 1) (list 2 (cdr poly6)) (list 0 1))))
(define poly2 (make-sparse-polynomial 'y (list (list 50 3) (list 2 (cdr poly7)) (list 0 3))))

;Exdrcise 2.95

;(define p1 (make-sparse-polynomial 'x '((2 1) (1 -2) (0 1))))
;(define p2 (make-sparse-polynomial 'x '((2 11) (0 7))))
;(define p3 (make-sparse-polynomial 'x '((1 13) (0 5))))
;(define q1 (mul p1 p2))
;(define q2 (mul p1 p3))

;Exercise 2.97

(define p1 (make-sparse-polynomial 'x '((1 1) (0 1))))
(define p2 (make-sparse-polynomial 'x '((3 1) (0 -1))))
(define p3 (make-sparse-polynomial 'x '((1 1))))
(define p4 (make-sparse-polynomial 'x '((2 1) (0 -1))))
(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p2 p4))




  


