#lang sicp

(define (power a n)
  (define (iter result n)
    (if (= n 1)
        result
        (iter (* result a) (- n 1))))
  (if (= n 0)
      1
      (iter a n)))

;local state variables

;(define balance 100)
;(define (withdraw amount)
;  (if (>= balance amount)
;      (begin (set! balance (- balance amount))
;             balance)
;      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100))
(define acc2 (make-account 100))

;Exercise 3.1

(define (make-accumulator amount)
  (lambda (term)
    (set! amount (+ amount term))
    amount))

(define A (make-accumulator 5))

;Exercise 3.2

(define (make-monitored f)
  (let ((counter 0))
    (define (how-many-calls?) counter)
    (define (reset-count)
      (begin (set! counter 0)
             counter))
    (define (mf x)
      (cond ((eq? x 'how-many-calls?) (how-many-calls?))
            ((eq? x 'reset-count) (reset-count))
            (else (begin (set! counter (+ counter 1))
                         (f x)))))
  mf))

(define s (make-monitored sqrt))

;Exercise 3.3

(define (new-make-account balance secret-password)
  (let ((counter 1))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch s-p m)
    (if (eq? s-p secret-password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT"
                           m)))
        (if (= counter 2)
            (lambda (x) call-the-cops)
            (begin (set! counter (+ counter 1))
                   (lambda (x) "Incorrect password")))))
  dispatch))

(define acc3 (new-make-account 100 'soda))
 
;Exercise 3.4

(define (emergency-call x)
  (cond ((= x 1) "Attention, fire")
        ((= x 2) "Attention, cops")))
(define call-the-cops (emergency-call 2))

;the benefits of introducing assigment

;(define rand (let ((x random-init))
;               (lambda ()
;                 (set! x (rand-update x))
;                 x)))

;(define (estimate-pi trials)
;  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
;(define (cesaro-test)
;  (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))
;(define (estimate-pi trials)
;  (sqrt (/ 6 (random-gcd-test trials random-init))))
;(define (random-gcd-test trials initial-x)
;  (define (iter trials-remaining trials-passed x)
;    (let ((x1 (rand-update x)))
;      (let ((x2 (rand-update x1)))
;        (cond ((= trials-remaining 0)
;               (/ trials-passed trials))
;              ((= (gcd x1 x2) 1)
;               (iter (- trials-remaining 1)
;                     (+ trials-passed 1)
;                     x2))
;              (else
;               (iter (- trials-remaining 1)
;                     trials-passed
;                     x2))))))
;  (iter trials 0 initial-x))

;Exercise 3.5

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
(define (predicate-P x1 x2 y1 y2)
  (let ((x (random-in-range x1 x2))
        (y (random-in-range y1 y2)))
    (<= (+ (power (- x 5.0) 2) (power (- y 7.0) 2)) 9.0)))
(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((experiment (lambda () (P x1 x2 y1 y2)))
        (area (* (- x2 x1) (- y2 y1))))
    (* area (monte-carlo trials experiment))))
(define (estimate-P P x1 x2 y1 y2 trials)
  (let ((S-c (estimate-integral P x1 x2 y1 y2 trials))
        (R (/ (- x2 x1) 2.0)))
    (/ S-c (power R 2.0))))

;Exercise 3.6

;(define new-rand
;  (let ((value random-init))
;    (lambda (action)
;      (cond ((eq? action 'generate)
;             (begin (set! value (rand-update value))
;                    value))
;            ((eq? action 'reset)
;             (lambda (x)
;               (begin (set! value x)
;                      value)))))))

;the costs of introducing assignment

(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))
(define W (make-simplified-withdraw 25))

(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))
(define D (make-decrementer 25))

;pitfalls of imperative programming

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product) (+ counter 1))))
  (iter 1 1))

(define (new-factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))

;Exercise 3.7

(define (new2-make-account balance secret-password)
  (let ((joint-password 'joint-password))
    (let ((counter 1))
      (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))
      (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
      (define (joint s-p)
        (set! joint-password s-p))
      (define (dispatch s-p m)
        (if (or (eq? s-p secret-password) (eq? s-p joint-password))
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  ((eq? m 'joint) joint)
                  (else (error "Unknown request: MAKE-ACCOUNT" m)))
            (if (= counter 10)
                (lambda (x) call-the-cops)
                (begin (set! counter (+ counter 1))
                       (lambda (x) "Incorrect password")))))
      dispatch)))

(define (make-joint2 account password joint-password)
  (begin ((account password 'joint) joint-password)
         account))

(define (make-joint account password joint-password)
  (define (dispatch s-p m)
    (if (eq? s-p joint-password)
        (cond ((eq? m 'withdraw) (account password 'withdraw))
              ((eq? m 'deposit) (account password 'deposit))
              (else (error "Unknown request: MAKE-JOINT" m)))
        (lambda (x) "Incorrect password")))
  dispatch)

(define peter-acc (new-make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

;((peter-acc 'open-sesame 'withdraw) 10)
;((paul-acc 'rosebud 'withdraw) 10)

;Exercise 3.8

;(define f
;  (let ((counter 0))
;    (lambda (amount)
;      (if (= counter 0)
;          (begin (set! counter (+ counter 1))
;                 amount)
;          (begin (set! counter 0)
;                 0)))))

;mutable list structure

;(define (new-cons x y)
;  (let ((new (get-new-pair)))
;    (set-car! new x)
;    (set-cdr! new y)
;    new))

;Exercise 3.12

;(define (append x y)
;  (if (null? x)
;      y
;      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

;(define x '(a b))
;(define y '(c d))
;(define w (append! x y))

;Exercise 3.13

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;Exercise 3.14

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;Exercise 3.16

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define x3 '(a b c))
(define x4 (list 'a (list 'b 'c)))
(define x7 (list 'a (list (list 'b) (list (list 'c)))))

;Exercise 3.17

(define count-pairs2
  (let ((counter 0))
    (lambda (x)
      (if (eq? x 'reset)
          (set! counter 0)
          (if (null? x)
              counter
              (if (not (pair? (car x)))
                  (begin (set! counter (+ counter 1))
                         (count-pairs2 (cdr x)))
                  (begin (count-pairs2 (car x))
                         (count-pairs2 (cdr x)))))))))

(define (count-pairs3 x)
  (count-pairs2 'reset)
  (count-pairs2 x))

;Exercise 3.19

(define (count-pairs1 x)
  (if (not (pair? x))
      0
      (+ (count-pairs1 (car x))
         (count-pairs1 (cdr x))
         (if (not (pair? (car x)))
             1
             0))))

;Exercise 3.18

(define (find-the-loop x)
  (define (iter y)
    (cond ((null? y) "no loop")
          ((eq? y x) "loop")
          (else (iter (cdr y)))))
  (iter (cdr x)))

;representing queues

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           
           (set-rear-ptr! queue new-pair)
           (set-front-ptr! queue (rear-ptr queue))
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))


          
;Exercise 3.21

(define q1 (make-queue))
(define q2 (make-queue))

(define (print-queue queue)
  (front-ptr queue))

;Exercise 3.22

(define (make-queue1)
  (let ((front-ptr (cons '() '()))
        (rear-ptr (cons '() '())))
    (define (insert pair)
      (let ((pair (cons pair nil)))
        (if (null? (car front-ptr))
            (begin (set! front-ptr pair)
                   (set! rear-ptr pair))
            (begin (set-cdr! rear-ptr pair)
                   (set! rear-ptr pair)))
        front-ptr))
    (define (delete)
      (if (null? (cdr front-ptr))
          (set! front-ptr (cons '() '()))
          (set! front-ptr (cdr front-ptr)))
      front-ptr)
    (define (dispatch m)
      (cond ((eq? m 'insert) insert)
            ((eq? m 'delete) (delete))
            (else (error "Unknown request" m))))
    dispatch))

;(define q2 (make-queue1))

;Exercise 3.23

(define (make-cell item)
  (cons (cons '() item) '()))
(define (cell-ptr cell)
  (car (car cell)))
(define (cell-item cell)
  (cdr (car cell)))
(define (set-cell-ptr! cell item)
  (set-car! (car cell) item))

(define (front-deque-ptr deque) (car deque))
(define (rear-deque-ptr deque) (cdr deque))
(define (empty-deque? deque)
  (null? (front-deque-ptr deque)))
(define (make-deque) (cons '() '()))
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty queue" deque)
      (cdr (car (front-deque-ptr deque)))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty queue" deque)
      (cdr (car (rear-deque-ptr deque)))))
(define (front-insert-deque! deque item)
  (let ((new-pair (make-cell item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           (set-cell-ptr! new-pair new-pair)
           deque)
          (else
           (set-cell-ptr! (front-ptr deque) new-pair)
           (set-cdr! new-pair (front-ptr deque))
           (set-front-ptr! deque new-pair)
           deque))))
(define (rear-insert-deque! deque item)
  (let ((new-pair (make-cell item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           (set-cell-ptr! new-pair new-pair)
           deque)
          (else
           (set-cell-ptr! new-pair (rear-ptr deque))
           (set-cdr! (rear-ptr deque) new-pair)
           (set-rear-ptr! deque new-pair)
           deque))))
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty queue" deque))
        (else (set-rear-ptr! deque (cell-ptr (rear-ptr deque)))
              (set-cdr! (rear-ptr deque) nil)
              deque)))
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty queue" deque))
        (else (set-front-ptr! deque (cdr (front-ptr deque)))
              (set-cell-ptr! (front-ptr deque) (front-ptr deque))
              deque)))
(define (print-deque deque)
  (display "first deque pair: ")
  (display (front-deque deque))
  (newline)
  (display "rear deque pair: ")
  (display (rear-deque deque)))

(define dq1 (make-deque))
;(front-insert-deque! dq1 'a)
;(rear-insert-deque! dq1 'a)
;(rear-delete-deque! dq1)
;(front-delete-deque! dq1)
;(print-deque dq1)

;representing tables

;one-dimentional tables

(define (lookup-1d key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
(define (insert-1d! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table)))))
  'ok)

(define (make-table-common)
  (list '*table*))
;(insert-1d! 1 'a t1)
;(insert-1d! 2 'b t1)
;(insert-1d! 3 'c t1)
;(lookup-1d 1 t1)
;(insert-list-1d! (list 1 2 3) 'wow t1)
;(lookup-list-1d (list 1 2 3) t1)

;two-dimentional tables

(define (lookup key-1 key-2 table)
  (let ((subtable
         (assoc key-1 (cdr table))))
    (if subtable
        (let ((record
               (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))
(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

;creating local tables

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))
                            
;Exercise 3.24

(define (keys-list-maker table)
  (define (iter list)
    (if (null? list)
        nil
        (cons (caar list)
              (iter (cdr list)))))
  (iter (cdr table)))
(define tolerance 0.5)
(define (same-key-1d? key table)
  (let ((keys-list (keys-list-maker table)))
    (define (iter list)
      (if (null? list)
          false
          (let ((item (car list)))
            (if (number? item)
                (let ((low (- item tolerance))
                      (high (+ item tolerance)))
                  (if (and (>= key low) (< key high))
                      item
                      (iter (cdr list))))
                (iter (cdr list))))))
    (iter keys-list)))
 
(define (make-table-sk)
  (let ((local-table (list '*table*))
        (key-1 'value)
        (key-2 'value))
    (define (lookup)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (same-key? key-sk-1 key-sk-2)
      (set! key-1 key-sk-1)
      (set! key-2 key-sk-2)
      (let ((new-key-1
             (same-key-1d? key-1 local-table)))
        (if new-key-1
            (let ((subtable
                   (assoc new-key-1 (cdr local-table))))
              (if subtable
                  (let ((new-key-2
                         (same-key-1d? key-2 subtable)))
                    (if new-key-2
                        (begin (set! key-1 new-key-1)
                               (set! key-2 new-key-2)
                               dispatch)
                        dispatch))
                  dispatch))
            dispatch)))
     (define (dispatch m)
      (cond ((eq? m 'lookup-proc) (lookup))
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'same-key?) same-key?)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

;(define tb1 (make-table-sk))
;((((tb1 'same-key?) 1 1) 'insert-proc!) 'a)
;((((tb1 'same-key?) 1 2) 'insert-proc!) 'b)
;((((tb1 'same-key?) 1 3) 'insert-proc!) 'c)
;((((tb1 'same-key?) 2 1) 'insert-proc!) 'd)
;((((tb1 'same-key?) 2 2) 'insert-proc!) 'e)
;((((tb1 'same-key?) 2 3) 'insert-proc!) 'f)
;((((tb1 'same-key?) 3 1) 'insert-proc!) 'g)
;((((tb1 'same-key?) 3 2) 'insert-proc!) 'h)
;((((tb1 'same-key?) 3 3) 'insert-proc!) 'i)

;((((tb1 'same-key?) 1.3 2.5) 'insert-proc!) 'wow)
;(((tb1 'same-key?) 1 3) 'lookup-proc)

;Exercise 3.25

(define (insert-list-1d! keys-list value table)
  (if (null? keys-list)
      'ok
      (begin (insert-1d! (car keys-list) value table)
             (insert-list-1d! (cdr keys-list) value table))))

(define (insert-list! keys-list-1 keys-list-2 value table)
  (if (null? keys-list-1)
      'done
      (let ((subtable (assoc (car keys-list-1) (cdr table))))
        (if subtable
            (begin (insert-list-1d! keys-list-2 value subtable)
                   (insert-list! (cdr keys-list-1) keys-list-2 value table))
            (begin (set-cdr! table
                             (cons (list (car keys-list-1))
                                   (cdr table)))
                   (insert-list! keys-list-1 keys-list-2 value table))))))
;(insert-list! (list 1 2 3) (list 1 2 3) 'a t1)

(define (lookup-list-1d keys-list table)
  (if (null? keys-list)
      nil
      (let ((key (car keys-list)))
        (let ((record (lookup-1d key table)))
          (if record
              (cons record
                    (lookup-list-1d (cdr keys-list) table))
              (lookup-list-1d (cdr keys-list) table))))))
(define (lookup-list keys-list-1 keys-list-2 table)
  (if (null? keys-list-1)
      nil
      (let ((key-1 (car keys-list-1)))
        (let ((record-1 (assoc key-1 (cdr table))))
          (if record-1
              (append (lookup-list-1d keys-list-2 record-1)
                      (lookup-list (cdr keys-list-1) keys-list-2 table))
              (lookup-list (cdr keys-list-1) keys-list-2 table))))))
;(lookup-list (list 1 3) (list 1 2 3) t1)

(define t1 (make-table-common))

;Exercise 3.26

(define (make-binary-table-tree entry left right)
  (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (assoc-binary key records)
  (cond ((null? records) false)
        ((= key (car (entry records))) (entry records))
        ((< key (car (entry records)))
         (assoc-binary key (left-branch records)))
        ((> key (car (entry records)))
         (assoc-binary key (right-branch records)))))
(define (insert-binary! key value table)
  (let ((record (assoc-binary key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (adjoin-binary (cons key value)
                                 (cdr table)))))
  'ok)
(define (adjoin-binary record records)
  (cond ((null? records)
         (make-binary-table-tree record '() '()))
        ((< (car record) (car (entry records)))
         (make-binary-table-tree (entry records)
                                 (adjoin-binary record (left-branch records))
                                 (right-branch records)))
        ((> (car record) (car (entry records)))
         (make-binary-table-tree (entry records)
                                 (left-branch records)
                                 (adjoin-binary record (right-branch records))))))
(define (lookup-binary key table)
  (let ((record (assoc-binary key (cdr table))))
    (if record
        (cdr record)
        false)))
;(insert-binary! 1 'a t1)
  
;a simulator for digital circuits

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
       and-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
(define (logical-and s1 s2)
  (cond ((or (and (= s1 1) (= s2 0))
             (and (= s1 0) (= s2 1))
             (and (= s1 0) (= s2 0)))
         0)
        ((and (= s1 1) (= s2 1)) 1)
        (else (error "Invalid signal" s1 s2))))

;Exercise 3.28

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay
       or-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
(define (logical-or s1 s2)
  (cond ((or (and (= s1 1) (= s2 0))
             (and (= s1 0) (= s2 1))
             (and (= s1 1) (= s2 1)))
         1)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" s1 s2))))

;define adders

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;Exercise 3.30

(define (ripple-carry-adder a-list b-list s-list c)
  (if (null? (cdr a-list))
      (full-adder (car a-list) (car b-list) c (car s-list) 0)
      (let ((c-n (make-wire)))
        (begin (full-adder (car a-list) (car b-list) c (car s-list) c-n)
               (ripple-carry-adder (cdr a-list) (cdr b-list) (cdr s-list) c-n)))))

;representing wires

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))
(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))
(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;the agenda

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))
(define (propagate)
  (if (empty-agenda? the-agenda)
      (begin (newline)
             'done)
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;a sample simulation

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

;implementing the agenda

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))
(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

;(define input-1 (make-wire))
;(define input-2 (make-wire))
;(define sum (make-wire))
;(define carry (make-wire))

;propagation of constraints

;implementing the constrain system

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product)
                          (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                      (/ (get-value product)
                         (get-value m2))
                      me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: MULTIPLIER"
                       request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (new-probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ") (display name)
    (display " = ") (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: PROBE" request))))
  (connect connector me)
  me)

;representing connectors

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: CONNECTOR"
                         request))))
    me))

(define (for-each-except exceptation procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exceptation) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

;using the constraint system

;(define c (make-connector))
;(define f (make-connector))
(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

;(celsius-fahrenheit-converter c f)
;(new-probe "Celsius temp" c)
;(new-probe "Fahrenheit temp" f)

;Exercise 3.33

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))
(define (averager a b c)
  (let ((d (make-connector))
        (e (make-connector)))
    (adder a b d)
    (multiplier d e c)
    (constant 0.5 e)
    'ok))

;(averager a b c)
;(new-probe "First addendum" a)
;(new-probe "Second addendum" b)
;(new-probe "Average" c)

;Exercise 3.34

;(define aa (make-connector))
;(define bb (make-connector))
;(define (squarer a b)
;  (multiplier a a b))
;(squarer aa bb)
;(new-probe "Multiplier" aa)
;(new-probe "Product" bb)

;Exercise 3.35

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: SQUARER"
                   (get-value b))
            (set-value! a
                        (sqrt (get-value b))
                        me))
        (if (has-value? a)
            (set-value! b
                        (power (get-value a) 2)
                        me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

;Exercise 3.37

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))
(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))
(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))
(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))
    
(define (new-celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
;(define c (make-connector))
;(define f (new-celsius-fahrenheit-converter c))

;complexity of using multiple shared resourdces

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

;(define (deposit account amount)
;  (let ((s (account 'serializer))
;        (d (account 'deposit)))
;    ((s d) amount)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

;implementing serializers

(define (make-serializer)
  (let ((mutex (make-mutex))) ;(make-semaphore 1)
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire) ;semaphore
        (let ((val (apply p args)))
          (mutex 'release) ;semaphore
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ;retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell false))
(define (test-and-set! cell)
  (if (car cell) true (begin (set-car! cell true) false)))

;Exercise 3.47

;part a

(define (make-semaphore n)
  (let ((lock (make-mutex))
        (counter 0))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (lock 'acquire)
             (if (< counter n)
                  (begin (lock 'release)
                         (lock 'acquire)
                         (set! counter (+ counter 1)))
                  (begin (lock 'release)
                         (the-semaphore 'acquire)))
             ((eq? m 'release)
              (lock 'acquire)
              (set! counter (- counter 1))
              (lock 'acquire)))))
    the-semaphore))

;part b

(define (make-semaphore-test n)
  (let ((cell (list false))
        (counter 0))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-semaphore 'acquire))
             (cond ((> counter n)
                    (clear! cell)
                    (the-semaphore 'acquire))
                   (else (set! counter (+ counter 1))
                         (clear! cell))))
            ((eq? m 'release)
             (if (test-and-set! cell)
                 (the-semaphore 'release))
             (set! counter (- counter 1))
             (clear! cell))))
    the-semaphore))

;Exercise 3.48

(define global-number 0)

(define (make-account-and-serializer-numbered balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer))
        (number (+ global-number 1)))
    (set! global-number number)
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'number) number)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))
                         
(define paul-acc1 (make-account-and-serializer-numbered 50))
;(define peter-acc1 (make-account-and-serializer-numbered 100))
      
(define (serialized-exchange-numbered account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (account-1-number (account1 'number))
        (account-2-number (account2 'number)))
    (if (< account-1-number account-2-number)
        ((serializer1 (serializer2 exchange))
         account1
         account2)
        ((serializer2 (serializer1 exchange))
         account1
         account2))))

;streams are delayed lists

(define (stream-null? x)
  (null? x))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      (begin (newline)
             'done)
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-n-times s n)
  (define (iter x)
    (if (= x n)
        (begin (newline)
               'done)
        (begin (newline)
               (display (stream-ref s x))
               (iter (+ x 1)))))
  (iter 0))
(define (display-line x) (newline) (display x))

;(define (cons-stream a b)
;  (cons a
;        (delay b)))
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream nil)

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

;prime implementation

(define (prime? n)
        (= n (smallest-divisor n)))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (square x) (* x x))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))


(define (probe11)
  (let ((start-time (runtime)))
    (begin (car (cdr (filter prime? (enumerate-interval 10000 1000000))))
           (- (runtime) start-time))))
(define (probe22)
  (let ((start-time (runtime)))
    (begin (stream-car (stream-cdr (stream-filter prime? (stream-enumerate-interval 10000 1000000))))
           (- (runtime) start-time))))

;Exercise 3.50

(define (stream-map-gen proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map-gen
              (cons proc (map stream-cdr argstreams))))))

(define stream-1
  (cons-stream 1
               (cons-stream 2
                            (cons-stream 3
                                         nil))))
(define stream-2
  (cons-stream 4
               (cons-stream 5
                            (cons-stream 6
                                         nil))))
(define stream-3
  (cons-stream 700
               (cons-stream 800
                            (cons-stream 900
                                         nil))))

;Exercise 3.51

(define (show x)
  (display-line x)
  x)
;(define x
;  (stream-map show
;              (stream-enumerate-interval 0 10)))
;(stream-ref x 5)
;(stream-ref x 7)

;Exercise 3.52

;(define sum 0)
;(define (accum x) (set! sum (+ x sum)) sum)
;(define seq
;  (stream-map accum
;              (stream-enumerate-interval 1 20)))
;(define y (stream-filter (lambda (x) (even? x)) seq))
;(define z1
;  (stream-filter (lambda (x) (= (remainder x 5) 0))
;                 seq))

;infinite streams

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))

;defining streams implicitly

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2) (stream-map-gen + s1 s2))
(define integers-new
  (cons-stream 1 (add-streams ones integers-new)))

(define fibs-new
  (cons-stream
   0
   (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes-new
  (cons-stream
   2
   (stream-filter prime-new? (integers-starting-from 3))))
(define (prime-new? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

;Exercise 3.53

(define ss (cons-stream 1 (add-streams ss ss)))

;Exercise 3.54

(define (mul-streams s1 s2) (stream-map-gen * s1 s2))
(define factorials
  (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))

;Exercise 3.55

(define (partial-sums stream)
  (cons-stream 0
               (add-streams (partial-sums stream) stream)))

;Exercise 3.56

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          (stream-cdr s2)))))))))

(define sss (cons-stream 1
                         (merge (scale-stream sss 2)
                                (merge (scale-stream sss 3)
                                       (scale-stream sss 5)))))

;Exercise 3.58

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;Exercise 3.59

;a

(define (div-streams s1 s2) (stream-map-gen / s1 s2))

(define den-integers
  (cons-stream 1
               (div-streams ones (stream-cdr integers))))

;(define (integrate-series poly)
;  (cons-stream (stream-car poly)
;               (mul-streams (stream-cdr poly)
;                            (stream-cdr den-integers))))

(define (integrate-series s)
  (stream-map-gen / s integers))

;b

(define minus-ones (cons-stream -1 minus-ones))

(define plus-minus-minus-plus
  (cons-stream 1
               (cons-stream -1
                            (cons-stream -1
                                         (cons-stream 1
                                                      plus-minus-minus-plus)))))
(define zeros-ones
  (cons-stream 0
               (cons-stream 1
                            zeros-ones)))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

;(define cosine-series
;  (cons-stream 1
;               (mul-streams (mul-streams plus-minus-minus-plus (integrate-series exp-series))
;                            zeros-ones)))
               

(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))                              

;Exercise 3.60

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

;Exercise 3.61

(define (invert-unit-series s)
  (cons-stream 1
               (scale-stream (mul-series (stream-cdr s)
                                         (invert-unit-series s))
                             -1)))

;Exercise 3.62
               
(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "the denominator has a zero constant term DIV-SERIES")
      (mul-series s1 (invert-unit-series s2))))

;exploiting the stream paradigm

(define average
  (lambda (x y) (/ (+ x y) 2)))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)


(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map-gen - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

;Exercise 3.63

(define (sqrt-stream-new x)
  (cons-stream 1.0
               (stream-map (lambda (guess) (sqrt-improve guess x))
                           (sqrt-stream-new x))))

;Exercise 3.64

(define (stream-limit s tolerance)
  (let ((s1 (stream-car s))
        (s2 (stream-car (stream-cdr s))))
    (if (>= tolerance (abs (- s1 s2)))
        s2
        (stream-limit (stream-cdr s) tolerance))))

(define (sqrt-tolerance x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;Exercise 3.65

(define (ln-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-summands (+ n 1)))))

(define ln-stream (partial-sums (ln-summands 1.0)))


  

                        
                        


  
      

    
        
      
        
  
  

