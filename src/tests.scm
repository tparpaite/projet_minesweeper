;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; board-scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CALANDRA Josephine
;; PARPAITE Thibault
;; PONCET Clemence
;; SARRABAYROUSE Alexis
;; Cree le 24/03/2017
;; Derniere modification le 12/05/2017
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require rackunit)
(require "board.scm")


;Test board-create
(define b1 (board-create "../graphes/graphe1.txt"))
(check-equal? (Board-size b1) 4 "The size of the board created is wrong")
(check-true (equal? (Cell-list-neighbors (vector-ref ( Board-tab b1 ) 0)) '(1 2)) "First cell's neighbors are not as specified in the file")
(check-true (equal? (Cell-list-neighbors (vector-ref ( Board-tab b1 ) 1)) '(0 3)) "Second cell's neighbors are not as specified in the file")
(check-true (equal? (Cell-list-neighbors (vector-ref ( Board-tab b1 ) 2)) '(0 3)) "Third cell's neighbors are not as specified in the file")
(check-true (equal? (Cell-list-neighbors (vector-ref ( Board-tab b1 ) 3)) '(1 2)) "Fourth cell's neighbors are not as specified in the file")

(define b2 (board-create "../graphes/graphe2.txt"))
(check-equal? (Board-size b2) 10 "The size of the board created is wrong")
(check-true (equal? (Cell-list-neighbors (vector-ref ( Board-tab b2 ) 0)) '(1 6)) "First cell's neighbors are not as specified in the file")
(check-true (equal? (Cell-list-neighbors (vector-ref ( Board-tab b2 ) 1)) '(0 2 5 6)) "Second cell's neighbors are not as specified in the file")
(check-true (equal? (Cell-list-neighbors (vector-ref ( Board-tab b2 ) 2)) '(1 3 4 5 6)) "Third cell's neighbors are not as specified in the file")
(check-true (equal? (Cell-list-neighbors (vector-ref ( Board-tab b2 ) 3)) '(2 4 5)) "Fourth cell's neighbors are not as specified in the file")
(check-true (equal? (Cell-list-neighbors (vector-ref ( Board-tab b2 ) 4)) '(2 3 5 7 8)) "Fifth cell's neighbors are not as specified in the file")
(check-true (equal? (Cell-list-neighbors (vector-ref ( Board-tab b2 ) 5)) '(1 2 3 4 6 7 8)) "Sixth cell's neighbors are not as specified in the file")
(check-true (equal? (Cell-list-neighbors (vector-ref ( Board-tab b2 ) 6)) '(0 1 2 5 8)) "Seventh cell's neighbors are not as specified in the file")
(check-true (equal? (Cell-list-neighbors (vector-ref ( Board-tab b2 ) 7)) '(4 5 8 9)) "Eighth cell's neighbors are not as specified in the file")
(check-true (equal? (Cell-list-neighbors (vector-ref ( Board-tab b2 ) 8)) '(4 5 6 7 9)) "9th cell's neighbors are not as specified in the file")
(check-true (equal? (Cell-list-neighbors (vector-ref ( Board-tab b2 ) 9)) '(7 8)) "10th cell's neighbors are not as specified in the file")

(define vide (board-create "../graphes/graphe_vide.txt"))
(check-equal? (Board-size vide) 0 "The size of the board created is wrong")


; Test cell-set-mine
(for ([i 4])
(check-false (Cell-mined? (vector-ref (Board-tab b1) i)) "The initialized board is already mined")
(check-eq? (Cell-n-mines-neighbors (vector-ref (Board-tab b1) i)) 0) "Number of mined neighbors is not initialized properly")
(set! b1 (cell-set-mine b1 3))
(check-true (Cell-mined? (vector-ref (Board-tab b1) 3)) "The given cell was not mined by cell-set-mine")
(check-false (Cell-mined? (vector-ref (Board-tab b1) 0)) "The function mined a cell thas it was not supposed to mine")
(check-false (Cell-mined? (vector-ref (Board-tab b1) 1)) "The function mined a cell thas it was not supposed to mine")
(check-false (Cell-mined? (vector-ref (Board-tab b1) 2)) "The function mined a cell thas it was not supposed to mine")
(check-eq? (Cell-n-mines-neighbors (vector-ref (Board-tab b1) 1)) 1 "The number of mined neighbors was not properly modified")
(check-eq? (Cell-n-mines-neighbors (vector-ref (Board-tab b1) 2)) 1 "The number of mined neighbors was not properly modified")
(check-eq? (Cell-n-mines-neighbors (vector-ref (Board-tab b1) 3)) 0 "The number of mined neighbors was not properly modified")
(check-eq? (Cell-n-mines-neighbors (vector-ref (Board-tab b1) 0)) 0 "The number of mined neighbors was not properly modified")


(for ([i 10])
(check-false (Cell-mined? (vector-ref (Board-tab b2) i))) "The initialized board was already mined")
(set! b2 (cell-set-mine b2 4))
(set! b2 (cell-set-mine b2 7))
(for ([i 4])
(check-false (Cell-mined? (vector-ref (Board-tab b2) i)) "The function mined a cell thas it was not supposed to mine"))
(check-true (Cell-mined? (vector-ref (Board-tab b2) 4)) "The given cell was not mined by cell-set-mine")
(for ([i (in-range 5 7)])
(check-false (Cell-mined? (vector-ref (Board-tab b2) i)) "The function mined a cell thas it was not supposed to mine"))
(check-true (Cell-mined? (vector-ref (Board-tab b2) 7)) "The given cell was not mined by cell-set-mine")
(for ([i (in-range 8 10)])
(check-false (Cell-mined? (vector-ref (Board-tab b2) i)) "The function mined a cell thas it was not supposed to mine"))
(check-eq? (Cell-n-mines-neighbors (vector-ref (Board-tab b2) 1)) 0 "The number of mined neighbors was not properly modified")
(check-eq? (Cell-n-mines-neighbors (vector-ref (Board-tab b2) 2)) 1 "The number of mined neighbors was not properly modified")
(check-eq? (Cell-n-mines-neighbors (vector-ref (Board-tab b2) 4)) 1 "The number of mined neighbors was not properly modified")
(check-eq? (Cell-n-mines-neighbors (vector-ref (Board-tab b2) 7)) 1 "The number of mined neighbors was not properly modified")
(check-eq? (Cell-n-mines-neighbors (vector-ref (Board-tab b2) 9)) 1 "The number of mined neighbors was not properly modified")
(check-eq? (Cell-n-mines-neighbors (vector-ref (Board-tab b2) 5)) 2 "The number of mined neighbors was not properly modified")
(check-eq? (Cell-n-mines-neighbors (vector-ref (Board-tab b2) 8)) 2 "The number of mined neighbors was not properly modified")




; Test cell-set-uncovered
(for ([i (Board-size b1)])
(check-false (Cell-uncovered? (vector-ref (Board-tab b1) i)) "The initialized board has some uncovered cells"))
(set! b1 (cell-set-uncovered b1 0))
(set! b1 (cell-set-uncovered b1 1))
(check-true (Cell-uncovered? (vector-ref (Board-tab b1) 0)) "The function did not uncover the cell it was supposed to")
(check-true (Cell-uncovered? (vector-ref (Board-tab b1) 1)) "The function did not uncover the cell it was supposed to")
(for ([i (in-range 2 4)])
(check-false (Cell-uncovered? (vector-ref (Board-tab b1) i)) "The function uncovered a cell it was not supposed to"))

(for ([i (Board-size b2)])
(check-false (Cell-uncovered? (vector-ref (Board-tab b2) i)) "The initialized board has some uncovered cells"))
(set! b2 (cell-set-uncovered b2 0))
(set! b2 (cell-set-uncovered b2 1))
(check-true (Cell-uncovered? (vector-ref (Board-tab b2) 0)) "The function did not uncover the cell it was supposed to")
(check-true (Cell-uncovered? (vector-ref (Board-tab b2) 1)) "The function did not uncover the cell it was supposed to")
(for ([i (in-range 2 10)])
(check-false (Cell-uncovered? (vector-ref (Board-tab b2) i)) "The function uncovered a cell it was not supposed to"))


; Test cell-set-flag :
(for ([i (Board-size b1)])
(check-eqv? (Cell-flag (vector-ref (Board-tab b1) i)) "none" "The initialized board already has some flagged cells"))
(set! b1 (cell-set-flag b1 2 "mined"))
(check-eqv? (Cell-flag (vector-ref (Board-tab b1) 2)) "mined" "The function did not flag the given cell correctly")
(for ([i (in-range 0 2)])
(check-eqv? (Cell-flag (vector-ref (Board-tab b1) i)) "none" "The function flagged a cell it was not supposed to"))
(check-eqv? (Cell-flag (vector-ref (Board-tab b1) 3)) "none" "The function flagged a cell it was not supposed to")
(set! b1 (cell-set-flag b1 3 "safe"))
(check-eqv? (Cell-flag (vector-ref (Board-tab b1) 3)) "safe" "The function did not flag the given cell correctly")
(for ([i (in-range 0 2)])
(check-eqv? (Cell-flag (vector-ref (Board-tab b1) i)) "none" "The function flagged a cell it was not supposed to"))
(check-eqv? (Cell-flag (vector-ref (Board-tab b1) 2)) "mined" "The function flagged a cell it was not supposed to")

(for ([i (Board-size b2)])
(check-eqv? (Cell-flag (vector-ref (Board-tab b2) i)) "none" "The initialized board already has some flagged cells"))
(set! b2 (cell-set-flag b2 8 "mined"))
(set! b2 (cell-set-flag b2 9 "mined"))
(check-eqv? (Cell-flag (vector-ref (Board-tab b2) 8)) "mined" "The function did not flag the given cell correctly")
(check-eqv? (Cell-flag (vector-ref (Board-tab b2) 9)) "mined" "The function did not flag the given cell correctly")
(for ([i (in-range 0 8)])
(check-eqv? (Cell-flag (vector-ref (Board-tab b2) i)) "none" "The function flagged a cell it was not supposed to"))
(set! b2 (cell-set-flag b2 6 "safe"))
(set! b2 (cell-set-flag b2 7 "safe"))
(check-eqv? (Cell-flag (vector-ref (Board-tab b2) 6)) "safe" "The function did not flag the given cell correctly")
(check-eqv? (Cell-flag (vector-ref (Board-tab b2) 7)) "safe" "The function did not flag the given cell correctly")
(for ([i (in-range 0 6)])
(check-eqv? (Cell-flag (vector-ref (Board-tab b2) i)) "none" "The function flagged a cell it was not supposed to"))
(check-eqv? (Cell-flag (vector-ref (Board-tab b2) 8)) "mined" "The function flagged a cell it was not supposed to")
(check-eqv? (Cell-flag (vector-ref (Board-tab b2) 9)) "mined" "The function flagged a cell it was not supposed to")


; Test cell-add-neighbor  : intérêt de la fonction?
(check-equal? (car(Cell-list-neighbors (vector-ref (Board-tab (cell-add-neighbor b1 0 3)) 0))) 3 "Neighbor was not added")
(check-equal? (car(Cell-list-neighbors (vector-ref (Board-tab (cell-add-neighbor b2 6 7)) 6))) 7 "Neighbor was not added")


;Test board-get-n-mines-neighbors
(check-equal? (board-get-n-mines-neighbors b1 1) 1 "Number of mined neighbors is wrong")
(check-equal? (board-get-n-mines-neighbors b1 0) 0 "Number of mined neighbors is wrong")
(for ([i (in-range 2 4)])
(check-equal? (board-get-n-mines-neighbors b1 i) -1 "The function gives an information it is suppose to keep from the user"))


(check-equal? (board-get-n-mines-neighbors b2 1) 0 "Number of mined neighbors is wrong")
(check-equal? (board-get-n-mines-neighbors b2 0) 0 "Number of mined neighbors is wrong")
(for ([i (in-range 2 10)])
(check-equal? (board-get-n-mines-neighbors b2 i) -1 "The function gives an information it is suppose to keep from the user "))



; Test board-generate-mines
; Visual verification
;(board-to-gv b1)
(set! b1 (board-generate-mines b1 1))
;(board-to-gv b1)


;(board-to-gv b2)
(set! b2 (board-generate-mines b2 3))
;(board-to-gv b2)


;Test board-uncover-cell

(set b2 (board-uncover-cell b2 5))
(check-true (Cell-uncovered? (vector-ref (Board-tab b2) 5)) "The function did not uncover the cell it was supposed to")
;(board-to-gv b2)
