#lang racket

;;; Project 0 Tic-tac-toe with Racket
;;; 
;;; Please immediately read README.md

(provide board?
         next-player
          valid-move?
          make-move
          winner?
          calculate-next-move)

;; 
;; Useful utility functions
;;

; Returns the number of elements in l for which the predicate f
; evaluates to #t. For example:
;
;    (count (lambda (x) (> x 0)) '(-5 0 1 -3 3 4)) => 3
;    (count (lambda (x) (= x 0)) '(-5 0 1 -3 3 4)) => 1
(define (count f l)
  (cond [(empty? l) 0]
        [(f (car l)) (add1 (count f (cdr l)))]
        [else (count f (cdr l))]))

;; 
;; Your solution begins here
;; 

; Check whether a list is a valid board
(define (square x)
  (* x x))

(define (length-square y)
  (integer? (sqrt (length y))))

(define (check-symbols lst-three)
  (equal? (+ (count (lambda (x) (equal? x 'X)) lst-three)
             (count (lambda (x) (equal? x 'E)) lst-three)
             (count (lambda (x) (equal? x 'O)) lst-three))
          (length lst-three)))

(define (differ-x-o lst-two)
  (<= (abs(- (count (lambda (x) (equal? x 'X)) lst-two)
             (count (lambda (x) (equal? x 'O)) lst-two)))
      1))

(define (x-first list-four)
  (>= (count (lambda (x) (equal? x 'X)) list-four)
      (count (lambda (x) (equal? x 'O)) list-four)))


(define (board? lst)
  (and (length-square lst)
       (check-symbols lst)
       (differ-x-o lst)
       (x-first lst)))
        
;;; From the board, calculate who is making a move this turn
(define (next-player board)
  (cond [(equal? (count (lambda (x) (equal? x 'X)) board)
                 (count (lambda (x) (equal? x 'O)) board))'X]
        [else 'O]))

;;; If player ('X or 'O) want to make a move, check whether it's this
;;; player's turn and the position on the board is empty ('E)
(define (player-turn? board player)
  (if (and (equal? player 'X)
           (equal? (next-player board) 'X)) #t
      (if (and (equal? player 'O)
               (equal? (next-player board) 'O)) #t #f)))

(define (board-location board row col)
  (if (and (equal? row 0) (equal? col 0)) 0
      (+ (* (sqrt (length board))row) (+ 1 col))))


(define (valid-move? board row col player)
  (if (equal? (board? board) #f) #f
      (if (< (length board) (board-location board row col)) #f
          (if (>= col (sqrt (length board))) #f
              (if (and (player-turn? board player) (equal? (list-ref board (board-location board row col)) 'E)) #t #f)))))

;;; To make a move, replace the position at row col to player ('X or 'O)
(define (make-move board row col player)
  (if (and (equal? row 0)
           (equal? col 0)) (list-set board 0 player)
                           (list-set board (- (board-location board row col) 1) player)))

;;; To determine whether there is a winner?


;; Row
(define (get-row? board)
  (let ([size (sqrt (length board))])
    (define (break-list lst)
      (if (empty? lst) '()
          (cons (take lst size)
                (break-list (drop lst size)))))
    (break-list board)))



;; Column
(define (get-column? board)
  (let ([size (sqrt (length board))])

    (define (comb-list board i)
      (if (= i 0) '()
          (cons (break-list board i) (comb-list board (- i 1)))))
          
    (define (break-list lst i)
      (if (empty? lst) '()
          (cons (list-ref lst (- i 1))
                (break-list (drop lst size) i))))
    (comb-list board size)))



;; Diagnal
(define (get-diag? board)
  (let ([size (sqrt (length board))])

    (define (comb-lst lst i)
      (if (> i size) '()
          (list (break-list lst 0 0) (break-list lst 1 (- size 1) ))))

    (define (break-list lst i k)
      (if (equal? 0 i)
          (if (empty? lst) '()
              (cons (list-ref lst k) (break-list (drop lst size) i (+ k 1))))
          (if (empty? lst) '()
              (cons (list-ref lst k) (break-list (drop lst size) i (- k 1))))))
    (comb-lst board 0)))



(define (all-player lst player)
  (cond [(empty? lst) #f]
        [(andmap (lambda (x) (equal? x player)) (first lst)) #t]
        [else (all-player (rest lst) player)]))
      

(define (winner? board)
  (cond [ (all-player (get-row? board) 'X) 'X]
      [(all-player (get-row? board) 'O) 'O]
      [(all-player (get-column? board) 'X) 'X]
      [(all-player (get-column? board) 'O) 'O]
      [(all-player (get-diag? board) 'X) 'X]
      [(all-player (get-diag? board) 'O) 'O]
      [else #f]))

;;; The board is the list containing E O X 
;;; Player will always be 'O
;;; returns a pair of x and y
(define (calculate-next-move board player)
  'todo)

