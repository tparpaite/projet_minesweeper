;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strategy_limited_search.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CALANDRA Josephine
;; PARPAITE Thibault
;; PONCET Clemence
;; SARRABAYROUSE Alexis
;; Cree le 14/04/2017
;; Derniere modification le 12/05/2017
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; (require racket/trace)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require "board.scm")
(require "strategy.scm")

(provide limited-search-strategy-prob)

(define (map-select f l pred)
  (append-map (lambda (x)
                (if (pred x)
                    (list (f x))
                    '()))
              l))


(define (remove-if pred l)
  (map-select identity l (lambda (x) (not (pred x)))))


(define (cell-covered b)
  (remove-if (lambda (c) (not (Cell-uncovered? c)))
             (vector->list (Board-tab b))))

                                                
(define (neighbors-covered b c_id)
  (remove-if (lambda (c) (not (Cell-uncovered? c)))
             (Cell-list-neighbors (vector-ref (Board-tab b) c_id))))


(define (neighbors-uncovered b c_id)
  (remove-if (lambda (c) (Cell-uncovered? c))
             (Cell-list-neighbors (vector-ref (Board-tab b) c_id))))

; S-squares in the algorithm
(define (neighbors-unflagged-uncovered b c_id)
  (remove-if (lambda (c) (or (not (Cell-uncovered? c)) (eqv? (Cell-flag c) "none")))
             (Cell-list-neighbors (vector-ref (Board-tab b) c_id))))

                                    
; S-info in the algorithm : label(c) - number of flagged cell adjacent to c
; pre : cell is covered
(define (n-unexplored-mines b c_id)
  (let ([c (vector-ref (Board-tab b) c_id)])
    (if (Cell-uncovered? c)
        (- (Cell-n-mines-neighbors c) (length (remove-if (lambda (cell) (not (eqv? (Cell-flag cell) "mine"))) (neighbors-uncovered b c_id))))
        (print "n-unexplored-mines : trying to acces uncovered mine"))))


;(: zone-of-interest (Board Integer -> (Listof Integer)))
(define (zone-of-interest b c_id)
  (append (neighbors-covered b c_id)
          (apply append (map (lambda (c_uncovered) (neighbors-covered b c_uncovered))
                             neighbors-uncovered))))

; Backtracking using depth-first-search
; [PARAMS]
; b : board
; O : O containing cells to treat
; NB : At the first call O contains all the unlabelled neighbours of all cells of Zone of interest (the first element must be x)
; bool : indicates whether the algorithm should place a mine on the next cell in O or leave it minefree
;
; [RETURN]
; #t if a contradiction has been found (the inverse move can be safely performed)
; #f otherwise (no contradiction found)
;
; (: backtracking-search (Board (Listof Cell) (Listof Cell) Boolean  -> Boolean))
(define (backtracking-search b O bool)
  (if (empty? O)
      #f ; No contradiction has been made on the current branch (end of recursion)  
      (let* ([c_id (Cell-id (car O))]
             [tmp-board-mine (if (bool)
                                 (functional-cell-set-flag b c_id "mined")       ; We are supposing that there is a mine at p position
                                 (functional-cell-set-flag b c_id "safe"))]) ; We are supposing that there is NO mine at p position
        ; Then browsing ZONE of interest to check if there is contradiction (empty = no contradiction atm, otherwise there is one)
        (if (empty? (remove-if (lambda (c) (or (>= (n-unexplored-mines b (Cell-id c)) 0)
                                               (>= (length (neighbors-unflagged-uncovered b (Cell-id c))) (n-unexplored-mines b c))))
                               (zone-of-interest tmp-board-mine c_id)))
            (and (backtracking-search tmp-board-mine (cdr O) #t) (backtracking-search tmp-board-mine (cdr O) #f))
            #t))))
      


; Backtracking using depth-first-search
; [PARAMS]
; b : board
; O : O containing cells to treat
; NB : At the first call O contains all the unlabelled neighbours of all cells of Zone of interest (the first element must be x)
; bool : indicates whether the algorithm should place a mine on the next cell in O or leave it minefree
;
; [RETURN]
; 1 if a contradiction has been found (the inverse move can be safely performed)
; 0 otherwise (no contradiction found)
;
; (: backtracking-search (Board (Listof Cell) (Listof Cell) Boolean  -> Boolean))
(define (backtracking-search-prob b O bool)
  (if (empty? O)
      0 ; No contradiction has been made on the current branch (end of recursion)  
      (let* ([c_id (Cell-id (car O))]
             [tmp-board-mine (if (bool)
                                 (functional-cell-set-flag b c_id "mined")       ; We are supposing that there is a mine at p position
                                 (functional-cell-set-flag b c_id "safe"))]) ; We are supposing that there is NO mine at p position
        ; Then browsing ZONE of interest to check if there is contradiction (empty = no contradiction atm, otherwise there is one)
        (if (empty? (remove-if (lambda (c) (or (>= (n-unexplored-mines b (Cell-id c)) 0)
                                               (>= (length (neighbors-unflagged-uncovered b (Cell-id c))) (n-unexplored-mines b c))))
                               (zone-of-interest tmp-board-mine c_id)))
            (+ (backtracking-search tmp-board-mine (cdr O) #t) (backtracking-search tmp-board-mine (cdr O) #f))
            1))))


(define (safe-move-update safe_move pair epsilon)
  (cond
    [(> (- (cadar safe_move) (cadr pair)) epsilon) safe_move]
    [(<= (abs (- (cadar safe_move) (cadr pair))) epsilon) (cons pair safe_move)]
    [(< (- (cadar safe_move) (cadr pair)) (- epsilon)) (list pair)]))


(define (limited-search-prob-bis b X safe_move)
  (if (empty? X)
      (cell-set-uncovered b (caar safe_move)) ; Safest move is played
      (let* ([O (remove-duplicates (cons (car X) (apply append (map (lambda (c_zone_interest) (neighbors-uncovered b c_zone_interest))))))]
             [m (backtracking-search-prob b O #t)]
             [f (backtracking-search-prob b O #f)])
        (cond
          [(= m 0) (cell-set-uncovered b (car X))] ; There is no mine at c_id
          [(= f 0) (cell-set-flag b (car X) "mined")] ; There is a mine at c_id
          [else (limited-search-prob-bis b (cdr X) (safe-move-update safe_move (list (car X) (+ m (/ (+ m f))))) 0.00001)])))) ; Trying another x in X


(define (limited-search-strategy-prob b)
  (limited-search-prob-bis b (cell-covered b) '()))

  



    
    
    
    
    

  
  