;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; board-scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CALANDRA Josephine
;; PARPAITE Thibault
;; PONCET Clemence
;; SARRABAYROUSE Alexis
;; Cree le 10/03/2017
;; Derniere modification le 20/04/2017
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; (require racket/trace)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

; struct Board |- size : Integer
;              |- n-mines : Integer
;              |- n-uncovered : Integer
;              |- tab : Vectorof Cell
(struct Board ((size)
               (n-mines)
               (n-uncovered)
               (tab)))


; struct Cell | - id : Integer
;             | - list-neighbors : Listof Integer
;             | - uncovered? : Boolean
;             | - mined? : Boolean
;             | - flag : String
;             | - n-mines-neighbors : Integer
(struct Cell ((id)
              (list-neighbors)
              (uncovered?)
              (mined?)
              (flag)
              (n-mines-neighbors)))

;Retrieves the information of file_name and process it into a string of characters

(provide
    file-to-string
    board-create
    board-grid-create
    cell-create
    board-get-n-mines-neighbors
    cell-set-mine
    cell-set-uncovered
    cell-set-flag
    functional-cell-set-flag
    cell-add-neighbor
    cell-add1-mine-neighbor
    board-generate-mines
    board-uncover-cell
    game-end
    board-all-uncovered
    path-gv-debug-output
    path-png-debug-output
    cells-to-gv
    board-to-gv

    (contract-out
        (struct Board (
            (size number?)
            (n-mines number?)
            (n-uncovered number?)
            (tab vector?)))
        (struct Cell (
            (id number?)
            (list-neighbors list?)
            (uncovered? boolean?)
            (mined? boolean?)
            (flag string?)
            (n-mines-neighbors number?))))
)

(require rackunit)

;Makes a string of character from the information given in file_name
;Used in board-create
;(: file-to-string (String -> Any))
(define (file-to-string file_name)
  (let ((p (open-input-file file_name)))
    (let f ((x (read p)))
      (if (eof-object? x)
          (begin
            (close-input-port p)
            "Empty file")
          x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Creates a new board using the information given by file_name
;tested
;(: board-create (String -> Board)
(define (board-create file_name)
  (let ([l (file-to-string file_name)])
    (Board (car l) 0 0 (apply vector (map (lambda (sub_l) (cell-create (car sub_l) (cadr sub_l))) (cdr l))))))


(define (board-grid-create r c)
  (define (generate-grid b c i size)
    (define (aux b size c_id neigh_id)
      (if (and (<= 0 neigh_id) (< neigh_id size))
          (cell-add-neighbor b c_id neigh_id)
          b))
    (if (< i size)
        (generate-grid
         (aux
          (aux
           (aux
            (aux
             (aux
              (aux
               (aux
                (aux b size i (add1 (- i c)))
                size i (add1 (+ i c)))
               size i (sub1 (- i c)))
              size i (sub1 (+ i c)))
             size i (- i c))
            size i (+ i c))
           size i (add1 i))
          size i (sub1 i))
         c
         (add1 i)
         size)
        b))
  (generate-grid
   (Board
    (* r c)
    0
    0
    (list->vector (map (lambda (x) (cell-create x '())) (build-list (* r c) identity))))
   c 0 (* r c)))
   


;Increments the number of mines of a board
;Used in cell-set-mine
;(: board-inc-mine (Board -> Board))
(define (board-inc-mine b)
  (Board
   (Board-size b)
   (add1 (Board-n-mines b))
   (Board-n-uncovered b)
   (Board-tab b)))

;Increments the number of uncovered cells of a board
;Used in cell-set-mine
;(: board-inc-uncovered (Board -> Board))
(define (board-inc-uncovered b)
  (Board
   (Board-size b)
   (Board-n-mines b)
   (add1 (Board-n-uncovered b))
   (Board-tab b))
  )


;Creates a new cell named id which list of neighbors is l
;Used in board-create
;(: cell-create (Integer (Listof Integer) -> Cell))
(define (cell-create id l)
  (Cell id l #f #f "none" 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Getter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Lets the player know how many of the c_id cell's neighbors are mined
; -But only if the c_id cell is uncovered. If it is not, the function returns -1.
; tested
;(: board-get-n-mines-neighbors (Board Integer -> Integer)
(define (board-get-n-mines-neighbors b c_id)
  (let ([c (vector-ref (Board-tab b) c_id)])
    (if (Cell-uncovered? c)
        (Cell-n-mines-neighbors c)
        -1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Setter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Makes a mined cell out of c_id
; tested
; used in board-generate-mines
;(: cell-set-mine (Board Integer -> Board))
(define (cell-set-mine b c_id)
  ; Communicates to its neighbor that c_id cell is now mined
  ;(: cell-update-neighbors (Board Integer -> Board))
  (define (cell-update-neighbors b c_id)
    (define (aux b l)
      (match l
        ('() b)
        ((cons h t) (aux (cell-add1-mine-neighbor b h) t))))
    (aux b (Cell-list-neighbors (vector-ref (Board-tab b) c_id))))
  (if (Cell-mined? (vector-ref (Board-tab b) c_id))
      b
      (let* ([c (vector-ref (Board-tab b) c_id)]
             [new_c (Cell c_id
                          (Cell-list-neighbors c)
                          (Cell-uncovered? c)
                          #t
                          (Cell-flag c)
                          (Cell-n-mines-neighbors c))])
        (vector-set! (Board-tab b) c_id new_c)
        
        (board-inc-mine (cell-update-neighbors b c_id)))))


; Uncovers c_id cell
; tested
;(: cell-set-uncovered (Board Integer -> Board))
(define (cell-set-uncovered b c_id)
  (let* ([c (vector-ref (Board-tab b) c_id)]
         [new_c (Cell c_id
                      (Cell-list-neighbors c)
                      #t
                      (Cell-mined? c)
                      (Cell-flag c)
                      (Cell-n-mines-neighbors c))])
    (vector-set! (Board-tab b) c_id new_c))
  (board-inc-uncovered b))



; Puts a flag on c_id which shows the player considers it to be a mined cell ("mine") or a safe one ("safe")
; tested
;(: cell-set-flag (Board Integer String -> Board))
(define (cell-set-flag b c_id flag)
  (check-true (or (eqv? flag "safe") (eqv? flag "none") (eqv? flag "mined")))
  (let ([c (vector-ref (Board-tab b) c_id)])
    (when (not (Cell-uncovered? c))
        (let ([new_c (Cell
                      c_id
                      (Cell-list-neighbors c)
                      (Cell-uncovered? c)
                      (Cell-mined? c)
                      flag
                      (Cell-n-mines-neighbors c))])
          (vector-set! (Board-tab b) c_id new_c)
          ))
    b))


(define (functional-cell-set-flag b c_id flag)
  (check-true (or (eqv? flag "safe") (eqv? flag "none") (eqv? flag "mined")))
  (let ([c (vector-ref (Board-tab b) c_id)])
    (when (not (Cell-uncovered? c))
      (let ([new_c (Cell
                    c_id
                    (Cell-list-neighbors c)
                    (Cell-uncovered? c)
                    (Cell-mined? c)
                    flag
                    (Cell-n-mines-neighbors c))])
        (Board
         (Board-size b)
         (Board-n-mines b)
         (Board-n-uncovered b)
         (vector-set! (vector-copy (Board-tab b)) c_id new_c)
         )))))


;; Public
;; Adds the neighbor_id cell to the c_id cell's list of neighbors
; used in board-create
; tested
;(: cell-add-neighbor (Board Integer Integer -> Board))
(define (cell-add-neighbor b c_id neighbor_id)
  (let* ([c (vector-ref (Board-tab b) c_id)]
         [new_c (Cell c_id
                      (cons neighbor_id (Cell-list-neighbors c)) ;; TODO : si
                      (Cell-uncovered? c)
                      (Cell-mined? c)
                      (Cell-flag c)
                      (Cell-n-mines-neighbors c))])
    (vector-set! (Board-tab b) c_id new_c))
  b)

;; Public
;; Increments the c_id cell's neighbors mines number
; Used in cell-set-mine
;(: cell-add1-mine-neighbor (Board Integer -> Board))
(define (cell-add1-mine-neighbor b c_id)
  (let* ([c (vector-ref (Board-tab b) c_id)]
         [new_c (Cell c_id
                      (Cell-list-neighbors c)
                      (Cell-uncovered? c)
                      (Cell-mined? c)
                      (Cell-flag c)
                      (add1 (Cell-n-mines-neighbors c)))])
    (vector-set! (Board-tab b) c_id new_c))
  b)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Randomly plants nb-mines mines into the board b given
; tested
;(: board-generate-mines (Board Integer -> Board))
(define (board-generate-mines b nb-mines)
  (define (aux b l nb-mines)
    (if (zero? nb-mines)
        b
        (match l
          ('() b)
          ((cons h t) (aux (cell-set-mine b h) t (sub1 nb-mines))))))
  (aux b (shuffle (build-list (Board-size b) identity)) nb-mines))


; Uncovers the c_id cell
; tested
;(: board-uncover-cell (Board Integer -> Board))
(define (board-uncover-cell b c_id )
  (let ([c (vector-ref (Board-tab b) c_id)])
    (if (or (Cell-uncovered? c)  (eqv? (Cell-flag c) "mined"))
        b
        (cond
          [(Cell-mined? c) (game-end b #f)]
          [(zero? (Cell-n-mines-neighbors c))
           (if (game-won (cell-set-uncovered b c_id))
               (game-end (cell-set-uncovered b c_id) #t)
               (foldl (λ (id new_b)
                        (board-uncover-cell new_b id))
                      (cell-set-uncovered b c_id)
                      (Cell-list-neighbors c)))]
          [else
           (if (game-won (cell-set-uncovered b c_id))
               (game-end (cell-set-uncovered b c_id) #t)
               (cell-set-uncovered b c_id))]
          ))))

;Tells the player if he lost or won the game and uncovers the whole board
; useful in board-uncover-cell
;(: game-end (Board Boolean -> Board))
(define (game-end b won)
  (cond
     [(not won) (print "GAME OVER")]
     [won (print "GAME WON")])
  (board-all-uncovered b))

;Uncovers the whole board
;useful in game-end
;(: board-all-uncovered Board -> Board))
(define (board-all-uncovered b)
  ( define (aux b l)
    (match l
           ('() b)
           ( (cons h t) (aux (cell-set-uncovered b (Cell-id h)) t))))
  (aux b (vector->list (Board-tab b))))

(define (game-won b)
  (equal? (Board-size b) (+ (Board-n-uncovered b) (Board-n-mines b))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Export to GraphViz
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define path-gv-debug-output "debug/graph1.gv")
(define path-png-debug-output "debug/graph1.png")


;(: cells-to-gv (Listof Cell -> void)
(define (cells-to-gv cells op_graph)
  (when (not (null? cells))
    (let ((c (car cells)))
      (begin
        (fprintf op_graph "~a [label = \"id = ~a | mine = " (Cell-id c) (Cell-id c))
        (if (Cell-mined? c)
            (fprintf op_graph "oui")
            (fprintf op_graph "non"))
        (fprintf op_graph " | flag = ")
        (if (eqv? (Cell-flag c) "mined")
            (fprintf op_graph "mined")
            (if (eqv? (Cell-flag c) "safe")
                (fprintf op_graph "safe")
                (fprintf op_graph "none")))
        (fprintf op_graph " | mines neigh = ~a\" " (Cell-n-mines-neighbors c))
        (if (Cell-uncovered? c)
            (when (Cell-mined? c)
              (fprintf op_graph "fillcolor=\"red\""))
            (if (Cell-mined? c)
                (fprintf op_graph "fillcolor=\"#ffbdbd\"")
                (fprintf op_graph "fillcolor=\"grey\"")))
        (fprintf op_graph "];~n")
        (cells-to-gv (cdr cells) op_graph)))))


;(: board-to-gv (Board -> Void))
(define (board-to-gv b)
  (define (conv-cells cells already op_graph)
    (define (conv-neighbors c neighbors already)
      (if (null? neighbors)
          (cons (Cell-id c) already)
          (if (not (member (car neighbors) already))
              (begin
                (fprintf op_graph "~a -- ~a;~n" (Cell-id c) (car neighbors))
                (conv-neighbors c (cdr neighbors) already))
              (conv-neighbors c (cdr neighbors) already))))
    (if (null? cells)
        already
        (conv-cells (cdr cells) (conv-neighbors (car cells) (Cell-list-neighbors (car cells)) already ) op_graph)))
  (let ((op_graph (open-output-file path-gv-debug-output #:exists 'truncate)))

    (fprintf op_graph "graph G {graph [rankdir = \"LR\"];node [shape = \"record\" style=filled fillcolor=\"white\"];~n")
    (cells-to-gv (vector->list (Board-tab b)) op_graph)
    (conv-cells (vector->list (Board-tab b)) '() op_graph)
    (fprintf op_graph "}~n")
    (close-output-port op_graph)
    ;(system "circo -Tpng graph1.gv -o graphcirco.png")
    ;(system "xdg-open graphcirco.png")))
    (system (~a "circo -Tpng "path-gv-debug-output " -o "path-png-debug-output))
    (system (~a "xdg-open "path-png-debug-output))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
