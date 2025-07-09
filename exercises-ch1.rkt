#lang racket
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (cube x) (* x x x))

(define (identity x) x)

(define (sum-integers-new a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;Exercise 1.31

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (new-product term a next b)
  (define (iter a product)
  (if (> a b)
      product
      (iter (next a) (* product (term a)))))
  (iter a 1))

(define (fterm x) x)

      (define (factorial b)
        (product fterm 1 inc b))

(define (inc-pi x) (+ x 2))
(define (square x) (* x x))
(define (odd? x)
  (= (remainder x 2) 1))

(define (pi b)
  (if (odd? b)
  (* 8
     (/
      (* (product square 4.0 inc-pi b) b)
      (product square 3.0 inc-pi b)))
  (pi (+ b 1))))

;Exercise 1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (ac-sum term a next b)
  (accumulate + 0 term a next b))

(define (new-accumulate combiner null-value term a next b)
  (define (iter a product)
    (if (> a b)
        product
        (iter (next a) (combiner product (term a)))))
  (iter a null-value))

;Exercise 1.33

(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (combiner (if (filter a)
                     (term a)
                     null-value)
                 (filtered-accumulate combiner null-value term (next a) next b filter))))

;prime procudure

(define (prime? n)
        (= n (smallest-divisor n)))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
;

(define (sum-sq-prime a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (prod-pos-prime b)
  (define (gcd? a) (= (gcd a b) 1))
  (filtered-accumulate * 1 identity 1 inc b gcd?))

;using let to create local variables

(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ (* x y))
   (- 1 y)))
(define (f1 x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(define (average x y)
  (/ (+ x y) 2))
(define (close-enough? x y)(< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

;Exercise 1.35

(define (fib-fp)
  (fixed-point (lambda (x) (+ 1.0 (/ 1.0 x)))
               1.0))

;Exercise 1.36

(define (x-x-1000)
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
               1.1))
(define (x-x-1000-ad)
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
               1.1))

;Exercise 1.37

(define (cont-frac n d k)
  (define (iter n x k)
      (let ((x (/ n x)))
        (if (= k 0)
          x
          (iter n (+ x (d k)) (- k 1)))))
  (iter (n k) (d k) k))

(define (cont-frac-rec n d k)
  (define (iter x k)
      (let ((x (/ n x)))
        (if (= k 0)
          x
          (/ n (+ d (iter x (- k 1)))))))
  (iter d k))

(define (cont-frac-1 k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

;Exercise 1.38

(define (e-seq k)
  (let ((x (/ (- k 2.0) 3.0)))
    (if (integer? x)
        (- k x)
        1.0)))

(define (de-frac-cont k)
  (cont-frac (lambda (i) 1.0) e-seq k))

;Exercise 1.39

(define (tan-seq x)
  (+ (* 2.0 x) 1.0))

(define (tan-cf x k)
  (define (iter result k)
  (let ((m (square x))
        (n (tan-seq result)))
    (let ((i (/ m n)))
      (if (= result k)
          i
          (/ m (- n (iter (+ result 1) k)))))))
  (/ x (- 1.0 (iter 1 k))))

;procedures as returned values

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt-ad x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.0001)

(define (cube-d x) (* x x x))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-nm x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-fpot x)
  (fixed-point-of-transform
   (lambda (y) (/ x y)) average-damp 1.0))

;Exercise 1.40

(define (double1 x) (* x x))
(define (cubic a b c)
  (newtons-method
   (lambda (x) (+ (cube x) (* a (double1 x)) (* b x) c)) -10.0))

(define (cubic1 a b c)
  (lambda (x) (+ (cube x) (* a (double1 x)) (* b x) c)))

;Exersise 1.41

(define (double x)
  (lambda (y) (x (x y))))

;Exercise 1.42

(define (compose f g)
  (lambda (y)
    (let ((x (g y)))
    (f x))))

;Exercise 1.43

(define (repeated f n)
  (lambda (y)
    (if (= n 1)
        (f y)
        (let ((x (f y)))
          (f x)))))

;Exercise 1.44

(define (average-of3 x y z)
  (/ (+ x y z) 3))
(define (smooth f)
  (lambda (x)
    (average-of3
     (f (- x dx))
     (f x)
     (f (+ x dx)))))

(define (n-fold-smooth f n)
  (repeated (smooth f) n))

;Exercise 1.45

(define (n-th-root x n i)
  (fixed-point (repeated
                (average-damp (lambda (y) (/ x (power y (- n 1)))))
                i)
               1.0))

(define (power a n)
  (define (iter result n)
    (if (= n 1)
        result
        (iter (* result a) (- n 1))))
  (iter a n))

;Exercise 1.46

(define (improving-method f guess)
  (let ((next (f guess)))
    (f next)))
(define (good-enough-method guess next)
  (< (abs (- guess next))
       tolerance))

(define (iterative-improve f guess)
  (define (iter guess)
    (let ((next (improving-method f guess)))
      (if (good-enough-method guess next)
        next
        (iter next))))
  (iter guess))
        
(define (sqrt-ii x)
  (iterative-improve (lambda (y) (average y (/ x y))) 1.1))

(define (fixed-point-ii)
  (iterative-improve))

(define (i-i g-e? i-m)
  (lambda (guess)
      (let ((next (i-m f guess)))
      (if (g-e? guess next)
        next
        (i-i g-e? (i-m f next))))))

(define (sqrt-i-i x)
  (let ((f (lambda (y) (average y (/ x y))))
        (guess 1.0))
    (i-i good-enough-method improving-method)))

;Exercise 2.37

(define (accumulate-new op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate-new op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate-new op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define v (list 1 3 -5))
(define w (list 4 -2 -1))

(define (dot-product v w)
  (accumulate-new + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons null mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

;(matrix-*-matrix (list (list 1 2 3) (list 4 5 6)) (list (list 7 8) (list 9 10) (list 11 12)))
  

  




