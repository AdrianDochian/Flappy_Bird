; Constante pentru joc
#lang racket
(require lang/posn)
(provide (all-defined-out))

;backup value 28.0
(define fps 58.0) 

(define scene-height 960)
(define scene-width 640)

(define ground-height (quotient scene-height 6))
(define ground-y (- scene-height ground-height))


;backup value 1.3
(define initial-gravity 0.5)

;backup balue 15.0
(define initial-momentum 12.0)

(define bird-width 81)
(define bird-height 57)
(define bird-x (quotient scene-width 3))
(define bird-initial-y (quotient scene-height 2))

(define pipe-width 104)
(define pipe-height scene-height)
(define pipe-gap 232.75)
(define pipe-self-gap 280)

;backup value 7.0
(define initial-scroll-speed 4.0)

(define added-number (quotient scene-height 8))
(define random-threshold (- (- scene-height (* 2 added-number)) pipe-self-gap))

(define no-pipes 6)
(define text-height 50)
(define text-x (- scene-width 50))
(define text-y 50)
(define text-posn (make-posn (- scene-width 50) 50))
(define SHOW_SCORE #t)
