;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strategy.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CALANDRA Josephine
;; PARPAITE Thibault
;; PONCET Clemence
;; SARRABAYROUSE Alexis
;; Cree le 10/03/2017
;; Derniere modification le 12/05/2017
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; (require racket/trace)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require "board.scm")


(provide
 n-flagged-neighbors)
 

; Returns the number of flagged neighbors
;(: n-flagged-neighbors (Board Integer -> Integer))
(define (n-flagged-neighbors b c_id)
  (define (aux b l n)
    (match l
      ('() n)
      ((cons h t) (if (eqv? (Cell-flag (vector-ref (Board-tab b) h)) "mine")
                      (aux b t (add1 n))
                      (aux b t n)))))
  (aux b (Cell-list-neighbors (vector-ref (Board-tab b) c_id)) 0))


; Flags the cells contained in l
;(: flag-cells (Board (Listof Integer) -> Board))
(define (flag-cells b l)
  (foldl (lambda (c_id b)
           (cell-set-flag b c_id))
         b l))

(define (uncover-cells b l)
  (foldl (lambda (c_id b)
           (board-uncover-cell b c_id))
         b l))
  
; Returns the number of hidden neighbors
;(: hidden-neighbors (Board Integer -> Integer))
(define (n-hidden-neighbors b c_id)
  (define (aux b l n)
    (match l
      ('() n)
      ((cons h t) (if (Cell-uncovered? (vector-ref (Board-tab b) h))
                      (aux b t n)
                      (aux b t (add1 n))))))
  (aux b (Cell-list-neighbors (vector-ref (Board-tab b) c_id)) 0))
  

; Uncovers and flags safe cells
;(: uncover-and-flag (Board Integer Cell (Listof Cell) -> Board)
; PRE-COND: start must be in already
(define (uncover-and-flag b start already)
; TODO : finir cette fonction, parcours toussa toussa
  (cond
    ((= (n-flagged-neighbors b start) (Cell-n-mines-neighbors (vector-ref (Board-tab b) start)))
     (uncover-cells b (Cell-list-neighbors (vector-ref (Board-tab b) start))))
    ((= (+ (n-flagged-neighbors b start) (n-hidden-neighbors b start)) (Cell-n-mines-neighbors (vector-ref (Board-tab b) start)))
     (flag-cells b (Cell-list-neighbors (vector-ref (Board-tab b) start))))
    (else b)))
     
  

;Returns the list of cells which are c's neighbors but not cv's
;(: select-neighbors-c-ncv (Board Integer Integer -> List of Integer )
( define (select-neighbors-c-ncv b c_id cv_id )
   (let ([c (vector-ref(Board-tab b) c_id)]
         [cv (vector-ref(Board-tab b) cv_id)])
     (vector-set! (Board-tab b) c_id c)
     (vector-set! (Board-tab b) cv_id cv) )
   (define (aux l b c cv )
     (if (findf (car (Cell-list-neighbors c)) (Cell-list-neighbors cv))
      (append l (car ( Cell-list-neighbors c)))
      l)
     (if (null? (cdr cv))
         l
         (aux ( l b ( cdr c ) ( cdr cv )))))
   (aux '() b c_id cv_id ))

; TODO: faire une fonction qui determine si les flags d'une Cell c sont aussi
;flags d'une Cell cv


