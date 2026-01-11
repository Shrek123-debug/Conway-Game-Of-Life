;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Jhon Conway's Game of Life|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require picturing-programs)

(define cell-size 20)
(define board-width 20)
(define board-height 20)

; Helper of update index 
(define (nth lst n)
  (cond
    [(zero? n) (first lst)]
    [else (nth (rest lst) (- n 1))]))
(check-expect (nth '(a b c) 1) 'b)

;; Replacer it seeks nth elemnt and what it does is that it replaces it with new (to replace cells)
(define (update-index lst n new)
  (cond
    [(zero? n) (cons new (rest lst))]
    [else (cons (first lst) (update-index (rest lst) (- n 1) new))]))
(check-expect (update-index '(a b c) 1 'x) '(a x c))

;; It dies or lives
(define (toggle cell)
  (if cell #f #t))
(check-expect (toggle #t) #f)
(check-expect (toggle #f) #t)

;; It dies or lives but in rows x and columns y
(define (update-cell board x y)
  (update-index board x
    (update-index (nth board x) y (toggle (nth (nth board x) y)))))
(check-expect (update-cell '((#f #f) (#f #f)) 0 1) '((#f #t) (#f #f)))

;;;;; MH spawns cell 
(define (mouse-handler board x y e)
  (if (mouse=? e "button-down")
      (update-cell board (quotient y cell-size) (quotient x cell-size))
      board))
(check-expect (mouse-handler '((#f #f) (#f #f)) 20 0 "button-down") '((#f #f) (#t #f)))

; In bounds 
(define (valid? x y)
  (and (>= x 0) (< x board-height)
       (>= y 0) (< y board-width)))
(check-expect (valid? 5 5) #t)
(check-expect (valid? -1 0) #f)

; return value of cell 
(define (get-cell board x y)
  (nth (nth board x) y))
(check-expect (get-cell '((#f #t) (#t #f)) 0 1) #t)

; self explanatory
(define (cell-alive? cell)
  (if cell 1 0))
(check-expect (cell-alive? #t) 1)
(check-expect (cell-alive? #f) 0)

;; Count live neighbors around cell 
(define (count-neighbors board x y)
  (+ (if (valid? (- x 1) (- y 1)) (cell-alive? (get-cell board (- x 1) (- y 1))) 0)
     (if (valid? (- x 1) y)       (cell-alive? (get-cell board (- x 1) y))       0)
     (if (valid? (- x 1) (+ y 1)) (cell-alive? (get-cell board (- x 1) (+ y 1))) 0)
     (if (valid? x       (- y 1)) (cell-alive? (get-cell board x      (- y 1))) 0)
     (if (valid? x       (+ y 1)) (cell-alive? (get-cell board x       (+ y 1))) 0)
     (if (valid? (+ x 1) (- y 1)) (cell-alive? (get-cell board (+ x 1) (- y 1))) 0)
     (if (valid? (+ x 1) y)       (cell-alive? (get-cell board (+ x 1) y))       0)
     (if (valid? (+ x 1) (+ y 1)) (cell-alive? (get-cell board (+ x 1) (+ 1 y))) 0)))
(define tiny-board '((#f #t #f) (#f #t #f) (#f #t #f)))
(check-expect (count-neighbors tiny-board 1 1) 2)

;; rules of game of life
(define (alive-ntick? board x y)
  (cond
    [(get-cell board x y) (if (or (< (count-neighbors board x y) 2) (> (count-neighbors board x y) 3)) #f #t)]
    [else (if (= (count-neighbors board x y) 3) #t #f)]))
(check-expect (alive-ntick? tiny-board 1 1) #t)

;; have cell in next row using alive-ntick
(define (new-row board i j)
  (cond
    [(>= j board-width) '()]
    [else (cons (alive-ntick? board i j)
                (new-row board i (+ j 1)))]))


;; Build full board by updating each row
(define (new-board board i)
  (cond
    [(>= i board-height) '()]
    [else (cons (new-row board i 0)
                (new-board board (+ i 1)))]))


; whole board
(define (NB board)
  (new-board board 0))
(check-expect (NB tiny-board) (new-board tiny-board 0))

; Cell Drawing
(define (cell->image cell)
  (overlay (rectangle cell-size cell-size "outline" "gray")
           (if cell
               (rectangle cell-size cell-size "solid" "black")
               (rectangle cell-size cell-size "solid" "white"))))
(check-expect (image? (cell->image #t)) #t)

; applies f to list parts
(define (applier f lst)
  (cond
    [(empty? lst) '()]
    [else (cons (f (first lst)) (applier f (rest lst)))]))
(check-expect (applier add1 '(1 2 3)) '(2 3 4))

(define (lis->row row)
  (beside-all (applier cell->image row)))
(check-expect (image? (lis->row '(#t #f))) #t)

(define (board->image board)
  (above-all (applier lis->row board)))
(check-expect (image? (board->image '((#t #f) (#f #t)))) #t)

(define (beside-all lst)
  (cond
    [(empty? (rest lst)) (first lst)]
    [else (beside (first lst) (beside-all (rest lst)))]))
(check-expect (beside-all (list (rectangle 10 10 "solid" "black")
                                (rectangle 10 10 "solid" "black")))
              (beside (rectangle 10 10 "solid" "black")
                      (rectangle 10 10 "solid" "black")))

(define (above-all lst)
  (cond
    [(empty? (rest lst)) (first lst)]
    [else (above (first lst) (above-all (rest lst)))]))
(check-expect (above-all (list (rectangle 10 10 "solid" "black")
                               (rectangle 10 10 "solid" "black")))
              (above (rectangle 10 10 "solid" "black")
                     (rectangle 10 10 "solid" "black")))

;; Makes amount of cells
(define (reproduce value n)
  (cond
    [(= n 0) '()]
    [else (cons value (reproduce value (- n 1)))]))
(check-expect (reproduce 'x 3) '(x x x))

;; Board initialization
(define empty-row (reproduce #f board-width))
(define empty-board (reproduce empty-row board-height))

(define board1 (update-cell empty-board 1 2))
(define board2 (update-cell board1 2 3))
(define board3 (update-cell board2 3 1))
(define board4 (update-cell board3 3 2))
(define board5 (update-cell board4 3 3))
(define initial-board board5)

;; Run the game
(big-bang initial-board
  (on-tick NB 0.5)
  (on-mouse mouse-handler)
  (on-draw board->image))

