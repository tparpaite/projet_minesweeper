#lang racket
(require "board.scm")
(require  2htdp/universe 2htdp/image htdp/gui)


(define b (board-grid-create 10 10))
(set b (board-generate-mines b 10))

 

(overlay
 (above (circle 10 "solid" "red") (circle 30 "solid" "yellow") (circle 50 "solid" "green"))
 (rectangle 100 150 "solid" "white"))

(define s1 (overlay (square 20 'outline 'black)(square 20 'solid 'white)))

(define f1 (overlay (circle 5 'solid 'orange) s1))
(define m1 (overlay (circle 5 'solid 'black) s1))
(define miss (overlay (circle 5 'solid 'red) s1))
(define sure (overlay (square 20 'outline 'black)(square 20 'solid 'green)))

(define r1 (apply beside (list (overlay (text "3" 11 'red) s1)  f1 sure miss m1 s1)))
(apply above (list r1 r1 r1 r1 r1 r1))

(define (create-UFO-scene height)
  (begin
    (printf "height : ~a~n~n" height)
    (overlay (apply above (list r1 r1 r1 r1 r1 r1)) (rectangle 400 350 'solid 'white) )))
 


(big-bang (circle 10 "solid" "red") (to-draw create-UFO-scene) (on-mouse (lambda (x c v b) (when (string=? "button-up" b)(printf "~a~n~a~n~a~n~a~n~n" x c v b)))))
