;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname snek) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

;; SNEK

(@htdw Game)

;; =================
;; Constants:

(define SQUARE-SIZE 20)
(define SQUARES-PER-SIDE 21)
(define MTS (square (* SQUARE-SIZE SQUARES-PER-SIDE) "solid" "black"))
(define CELL (square SQUARE-SIZE "solid" "white"))

;; =================
;; Data definitions:

(@htdd Dir)
;; Dir is one of:
;; - "U"
;; - "D"
;; - "L"
;; - "R"
;; interp. The direction the snek is heading in

(define D1 "R")

(define (fn-for-dir d)
  (cond [(string=? "U" d) ...]
        [(string=? "D" d) ...]
        [(string=? "L" d) ...]
        [else ...]))

(@htdd Cell)
(define-struct cell (x y))
;; cell is (make-cell Natural Natural) 
;; interp. The x y position of a cell
;; CONSTRAINT: cell-x and cell-y are in (0,SQUARES-PER-SIDE)

(define S1 (make-cell 11 11))
(define S2 (make-cell 12 11))
(define F1 (make-cell 16 11))

(define (fn-for-cell p)
  (make-cell (cell-x p)
             (cell-y p)))

(define (fn-for-lop lop)
  (cons (fn-for-cell (first lop))
        (fn-for-lop (rest lop))))

(@htdd Snek)
(define-struct snek (lop dir))
;; Snek is (make-snek (listof cell) Dir)
;; interp. The Snek made of a list of cells and a direction it's heading in

(define SNEK1 (make-snek (list S1) D1))
(define SNEK2 (make-snek (list S2) D1))
(define SNEK3 (make-snek (list S1 S2) D1))

(define (fn-for-snek s)
  (make-snek (fn-for-lop (snek-lop s))
             (fn-for-dir (snek-dir s))))



(@htdd Game)
(define-struct game (snek food))
;; Game is (make-game Snek cell)
;; interp. The status of the current game with the snek and a cell of food

(define G1 (make-game SNEK1 F1))
(define G2 (make-game SNEK3 F1))

(define (fn-for-game g)
  (make-game (fn-for-snek (game-snek g))
             (fn-for-cell (game-food g))))


;; =================
;; Functions:

(@htdf main)
(@signature Game -> Game)
;; start the world with (main G1)
;; 

(@template htdw-main)

(define (main g)
  (big-bang g                ; Game
    (on-tick next-game 0.5)  ; Game -> Game
    (to-draw render-game)    ; Game -> Image
    ;(stop-when ...)         ; Game -> Boolean
    ;(on-mouse  ...)         ; Game Integer Integer MouseEvent -> Game
    (on-key handle-key)))    ; Game KeyEvent -> Game

(@htdf next-game)
(@signature Game -> Game)
;; Produce the next game status by moving snek and food
(check-expect (next-game G1)
              (make-game (make-snek (list S2) D1) F1))

(@template Game)
(define (next-game g)
  (local [(define eat-food? 
            (equal? (first (snek-lop (game-snek g))) (game-food g)))]
    
    (make-game (next-snek (game-snek g) eat-food?) 
               (if eat-food?
                   (next-food (game-snek g))
                   (game-food g)))))
                  

(@htdf next-snek)
(@signature Snek Boolean -> Snek)
;; Produce the next Snek by moving each of its cells
(check-expect (next-snek SNEK1 false) SNEK2)

(define (next-snek s ef)
  (local [(define nhead (next-head (first (snek-lop s)) (snek-dir s)))
          
          (define nlop (cond [ef
                              (cons nhead (snek-lop s))]
                             [(cell=? (first (snek-lop s)) nhead)
                              (cons nhead (rest (snek-lop s)))]
                             [else
                              (cons nhead (next-body (rest (snek-lop s))
                                                     (first (snek-lop s))))]))]
    
    (make-snek (if (member? nhead (snek-lop s))
                   (snek-lop s)
                   nlop)
               (snek-dir s)))) 

(@htdf next-head)
(@signature Cell Dir -> Cell)
;; produce the next head of the Snek by moving it in the current direction
(check-expect (next-head S1 "R") (make-cell 12 11))
(check-expect (next-head (make-cell 12 11) "L") (make-cell 11 11))
(check-expect (next-head (make-cell 11 11) "U") (make-cell 11 10))
(check-expect (next-head (make-cell 11 10) "D") (make-cell 11 11))
(check-expect (next-head (make-cell 1 11) "L") (make-cell 1 11))
(check-expect (next-head (make-cell 2 11) "L") (make-cell 1 11))
(check-expect (next-head (make-cell SQUARES-PER-SIDE 11) "R")
              (make-cell SQUARES-PER-SIDE 11))
(check-expect (next-head (make-cell (- SQUARES-PER-SIDE 1) 11) "R")
              (make-cell SQUARES-PER-SIDE 11))
(check-expect (next-head (make-cell 11 1) "U") (make-cell 11 1))
(check-expect (next-head (make-cell 11 2) "U") (make-cell 11 1))
(check-expect (next-head (make-cell 11 SQUARES-PER-SIDE) "D")
              (make-cell 11 SQUARES-PER-SIDE))
(check-expect (next-head (make-cell 11 (- SQUARES-PER-SIDE 1)) "D")
              (make-cell 11 SQUARES-PER-SIDE))
    
(define (next-head p d)
  (local [(define (move-left p)
            (if (<= (cell-x p) 1)
                (make-cell 1 (cell-y p))
                (make-cell (- (cell-x p) 1) (cell-y p))))

          (define (move-right p)
            (if (>= (cell-x p) SQUARES-PER-SIDE)
                (make-cell SQUARES-PER-SIDE (cell-y p))
                (make-cell (+ (cell-x p) 1) (cell-y p))))

          (define (move-up p)
            (if (<= (cell-y p) 1)
                (make-cell (cell-x p) 1)
                (make-cell (cell-x p) (- (cell-y p) 1))))

          (define (move-down p)
            (if (>= (cell-y p) SQUARES-PER-SIDE) 
                (make-cell (cell-x p) SQUARES-PER-SIDE)
                (make-cell (cell-x p) (+ (cell-y p) 1))))]
    (cond [(string=? "U" d) (move-up p)]
          [(string=? "D" d) (move-down p)]
          [(string=? "L" d) (move-left p)]
          [else (move-right p)]))) 


(@htdf next-body)
(@signature (listof Cell) Cell -> (listof Cell))
;; Produce next body of snake by moving each cell to follow the next one
(check-expect (next-body empty S1) empty)
(check-expect (next-body (list S2) S1) (list S1)) 

(define (next-body lop lp)
  (cond [(empty? lop) empty]
        [else
         (cons lp (next-body (rest lop) (first lop)))]))

(@htdf next-food)
(@signature Snek -> Cell)
;; Produce the next food pos by moving it if it has been eaten by head of snek
(check-random (next-food SNEK3)
              (make-cell (+ (random (- SQUARES-PER-SIDE 1)) 1)
                         (+ (random (- SQUARES-PER-SIDE 1)) 1)))
;; EXAMPLE FROM GREGOR
;;
;; (define (new-food gs)
;;  (local [(define p (random-position))]
;;   (if (not (touches? p (rest (gs-snake gs))))
;;       p
;;       (new-food gs))))

(@template Cell) 
(define (next-food s)
  (local [(define f
            (make-cell (+ (random (- SQUARES-PER-SIDE 1)) 1)
                       (+ (random (- SQUARES-PER-SIDE 1)) 1)))]
    (if (member? f (snek-lop s))
        (next-food s)
        f)))

(@htdf render-game)
(@signature Game -> Image)
;; Render the game including the snek head and food 
(check-expect (render-game G1)
              (place-image CELL
                           (- (* 16 SQUARE-SIZE) (/ SQUARE-SIZE 2))
                           (- (* 11 SQUARE-SIZE) (/ SQUARE-SIZE 2))
                           (place-image CELL
                                        (- (* 11 SQUARE-SIZE) (/ SQUARE-SIZE 2))
                                        (- (* 11 SQUARE-SIZE) (/ SQUARE-SIZE 2))
                                        MTS)))

(@template Game)
(define (render-game g)
  (render-lot (cons (game-food g) (snek-lop (game-snek g)))))

(@htdf render-lot)
(@signature (listof Cell) -> Image)
;; Render a list of cells onto the scene

(define (render-lot lot)
  (cond [(empty? lot) MTS]
        [else
         (local [(define (cell-pos n)
                   (- (* n SQUARE-SIZE) (/ SQUARE-SIZE 2)))]
           (place-image CELL
                        (cell-pos (cell-x (first lot)))
                        (cell-pos (cell-y (first lot)))
                        (render-lot (rest lot))))]))

(@htdf handle-key)
(@signature Game KeyEvent -> Game)
;; Change the direction of the Snek with the arrow keys
(check-expect (handle-key G1 "right") G1)
(check-expect (handle-key G1 "left")
              (make-game (make-snek (list S1) "L") F1))
(check-expect (handle-key G1 "up") 
              (make-game (make-snek (list S1) "U") F1))
(check-expect (handle-key G1 "down")
              (make-game (make-snek (list S1) "D") F1))
(check-expect (handle-key G1 "q") G1)
            

(define (handle-key g ke)
  (make-game (make-snek (snek-lop (game-snek g))
                        (cond [(string=? ke "up") "U"]
                              [(string=? ke "down") "D"]
                              [(string=? ke "left") "L"]
                              [(string=? ke "right") "R"]
                              [else (snek-dir (game-snek g))]))
             (game-food g)))



(define (cell=? c1 c2)
  (and (= (cell-x c1) (cell-x c2)) (= (cell-y c1) (cell-y c2))))
