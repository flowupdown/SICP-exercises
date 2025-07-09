#lang racket
(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))



;frames

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

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

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

;need a wrapper function so that the graphics library works with my code...
(define (vector-to-posn v)
  (make-posn (car v) (cdr v)))

(define (segments->painter segment-list)   
  (lambda (frame)     
   (for-each     
     (lambda (segment)        
      (line         
        (vector-to-posn ((frame-coord-map frame) (start-segment segment)))         
        (vector-to-posn ((frame-coord-map frame) (end-segment segment)))))      
      segment-list)))

;((draw-line vp) (make-posn 30 30) (make-posn 100 100))

;Exercise 2.48

(define (make-segment start-segment end-segment)
  (list start-segment end-segment))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (car (cdr segment)))

;Exersise 2.49

;outline+x
((segments->painter
 (list (make-segment (make-vect 0 0)
                     (make-vect 1 0))
       (make-segment (make-vect 1 0)
                     (make-vect 1 1))
       (make-segment (make-vect 1 1)
                     (make-vect 0 1))
       (make-segment (make-vect 0 1)
                     (make-vect 0 0))
       (make-segment (make-vect 0 0)
                     (make-vect 1 1))
       (make-segment (make-vect 0 1)
                     (make-vect 1 0))))
 (make-frame (make-vect 100 200)
             (make-vect 100 50)
             (make-vect -50 100)))
(clear)

;peace
(define peace
  (segments->painter
   (list (make-segment (make-vect 0 0)
                       (make-vect 1 0))
         (make-segment (make-vect 1 0)
                       (make-vect 1 1))
         (make-segment (make-vect 1 1)
                       (make-vect 0 1))
         (make-segment (make-vect 0 1)
                       (make-vect 0 0))
         (make-segment (make-vect 0.5 0.5)
                       (make-vect 1 1))
         (make-segment (make-vect 0.5 0.5)
                       (make-vect 0 0.5))
         (make-segment (make-vect 0.5 0)
                       (make-vect 0.5 1)))))
(peace
 (make-frame (make-vect 0 0)
             (make-vect 100 0)
             (make-vect 0 100)))
(clear)

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))

(define (flip-vert-trans painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 1 1)
                     (make-vect 0 0)))
((flip-vert-trans peace) (make-frame (make-vect 0 0)
                                     (make-vect 100 0)
                                     (make-vect 0 100)))
(clear)

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1 0.5)
                     (make-vect 0.5 1)))
((shrink-to-upper-right peace) (make-frame (make-vect 0 0)
                                           (make-vect 100 0)
                                           (make-vect 0 100)))
(clear)

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 1 1)
                     (make-vect 0 0)))
((rotate90 peace) (make-frame (make-vect 0 0)
                              (make-vect 100 0)
                              (make-vect 0 100)))
(clear)

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0 0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))
((squash-inwards peace) (make-frame (make-vect 0 0)
                                    (make-vect 100 0)
                                    (make-vect 0 100)))
(clear)

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0)))
    (let ((paint-left
           (transform-painter
            painter1
            (make-vect 0 0)
            split-point
            (make-vect 0 1)))
          (paint-right
           (transform-painter
            painter2
            split-point
            (make-vect 1 0)
            (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))
((beside peace peace) (make-frame (make-vect 0 0)
                                  (make-vect 100 0)
                                  (make-vect 0 100)))
(clear)

;Exercise 2.50

(define (flip-horiz-trans painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))
((flip-horiz-trans peace) (make-frame (make-vect 0 0)
                                      (make-vect 100 0)
                                      (make-vect 0 100)))
(clear)

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1 1)
                     (make-vect 0 1)
                     (make-vect 1 0)))
((rotate180 peace) (make-frame (make-vect 0 0)
                               (make-vect 100 0)
                               (make-vect 0 100)))
(clear)

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 1 1)
                     (make-vect 0 0)))
((rotate270 peace) (make-frame (make-vect 0 0)
                               (make-vect 100 0)
                               (make-vect 0 100)))
(clear)

;Exercise 2.51

(define (below painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-lower
           (transform-painter
            painter1
            split-point
            (make-vect 1 0.5)
            (make-vect 0 1)))
          (paint-upper
           (transform-painter
            painter2
            (make-vect 0 0)
            (make-vect 1 0)
            split-point)))
      (lambda (frame)
        (paint-lower frame)
        (paint-upper frame)))))
((below peace peace) (make-frame (make-vect 0 0)
                                 (make-vect 100 0)
                                 (make-vect 0 100)))
(clear)

(define (below-new painter1 painter2)
  (rotate180 (rotate270 (beside (rotate270 painter1) (rotate270 painter2)))))
((below-new peace peace) (make-frame (make-vect 0 0)
                                 (make-vect 100 0)
                                 (make-vect 0 100)))
(clear)

;Exercise 2.52

(define peace-for-all
  (segments->painter
   (list (make-segment (make-vect 0 0)
                       (make-vect 1 0))
         (make-segment (make-vect 1 0)
                       (make-vect 1 1))
         (make-segment (make-vect 1 1)
                       (make-vect 0 1))
         (make-segment (make-vect 0 1)
                       (make-vect 0 0))
         (make-segment (make-vect 0.5 0.5)
                       (make-vect 1 1))
         (make-segment (make-vect 0.5 0.5)
                       (make-vect 0 0.5))
         (make-segment (make-vect 0.5 0)
                       (make-vect 0.5 1))
         (make-segment (make-vect 0.05 0.1)
                       (make-vect 0.15 0.2))
         (make-segment (make-vect 0.15 0.1)
                       (make-vect 0.05 0.2))
         (make-segment (make-vect 0.35 0.1)
                       (make-vect 0.45 0.2))
         (make-segment (make-vect 0.45 0.1)
                       (make-vect 0.35 0.2))
         (make-segment (make-vect 0.05 0.4)
                       (make-vect 0.15 0.3))
         (make-segment (make-vect 0.15 0.3)
                       (make-vect 0.25 0.4))
         (make-segment (make-vect 0.25 0.4)
                       (make-vect 0.35 0.3))
         (make-segment (make-vect 0.35 0.3)
                       (make-vect 0.45 0.4)))))
(peace-for-all (make-frame (make-vect 0 0)
                                 (make-vect 100 0)
                                 (make-vect 0 100)))
(clear)
(define kira
  (segments->painter
   (list (make-segment (make-vect 0.5 0)
                       (make-vect 0.5 1))
         (make-segment (make-vect 0 0.5)
                       (make-vect 1 0.5))
         (make-segment (make-vect 0 0)
                       (make-vect 1 1))
         (make-segment (make-vect 1 0)
                       (make-vect 0 1))
         )))
((below (rotate90 (below kira kira)) (below kira kira)) (make-frame (make-vect 0 0)
                                                                    (make-vect 500 0)
                                                                    (make-vect 0 500)))
(clear)





