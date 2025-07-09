#lang racket
(require sicp-pict)
(define einstein2 (beside einstein (flip-vert einstein)))
(define einstein4 (below einstein2 einstein2))
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))
(define (flipped-pairs1 painter)
  (let ((combine4 (square-of-four identity flip-vert
                                 identity flip-vert)))
    (combine4 painter)))
(define (square-limit1 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;Exercise 2.45

(define (split first-split second-split)
  (lambda (painter n)
    (define (iter n)
      (if (= n 0)
          painter
          (let ((smaller (iter (- n 1))))
            (first-split painter (second-split smaller smaller)))))
    (iter n)))
(define right-split-new (split beside below))
(define up-split-new (split below beside))

;frames

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame-new frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame-new frame))
               (scale-vect (ycor-vect v) (edge2-frame-new frame))))))

;Exercise 2.46

(define (make-vect xcor-vect ycor-vect)
  (cons xcor-vect ycor-vect))
(define (xcor-vect vect)
  (car vect))
(define (ycor-vect vect)
  (cdr vect))
(define (add-vect vect1 vect2)
  (let ((x1 (xcor-vect vect1))
        (x2 (xcor-vect vect2))
        (y1 (ycor-vect vect1))
        (y2 (ycor-vect vect2)))
    (make-vect (+ x1 x2) (+ y1 y2))))
(define (sub-vect vect1 vect2)
  (let ((x1 (xcor-vect vect1))
        (x2 (xcor-vect vect2))
        (y1 (ycor-vect vect1))
        (y2 (ycor-vect vect2)))
    (make-vect (- x1 x2) (- y1 y2))))
(define (scale-vect s vect)
  (let ((x (xcor-vect vect))
        (y (ycor-vect vect)))
    (make-vect (* s x) (* s y))))

;Exercise 2.47

(define (pick-pair list1 n)
  (cond ((> n (length list1))
         (error "value out of region"))
        ((= n 1) (car list1))
        (else
         (pick-pair (cdr list1) (- n 1)))))
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)
  (pick-pair frame 1))
(define (edge1-frame frame)
  (pick-pair frame 2))
(define (edge2-frame frame)
  (pick-pair frame 3))

(define (make-frame-new origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame-new frame)
  (car frame))
(define (edge1-frame-new frame)
  (car (cdr frame)))
(define (edge2-frame-new frame)
  (cdr (cdr frame)))
         


;painters

(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

;need a wrapper function so that the graphics library works with my code...
(define (vector-to-posn v)
  (make-posn (car v) (car(cdr v))))

(define (segments->painter segment-list)
  (lambda (frame)  
   (for-each  
     (lambda (segment)    
      (line    
        (vector-to-posn ((frame-coord-map frame) (start-segment segment)))         
        (vector-to-posn ((frame-coord-map frame) (end-segment segment)))))      
      segment-list)))

;Exercise 2.48

(define (make-segment start-segment end-segment)
  (list start-segment end-segment))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (car (cdr segment)))

;Exersise 2.49

;a
(define my-vect-orig (make-vect 0.5 0.5))
(define my-vect1 (make-vect 1 1))
(define my-vect2 (make-vect 0.5 1))
(define my-frame (make-frame my-vect-orig my-vect1 my-vect2))
(define my-segment (make-segment my-vect1 my-vect2))



     
