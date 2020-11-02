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
(define TILE (square SQUARE-SIZE "solid" "white"))

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

(@htdd Tile)
(define-struct tile (x y))
;; tile is (make-tile Natural Natural)
;; interp. The x y position of a tile
;; CONSTRAINT: tile-x and tile-y are in (0,SQUARES-PER-SIDE)

(define S1 (make-tile 11 11))
(define S2 (make-tile 12 11))
(define F1 (make-tile 16 11))

(define (fn-for-tile p)
  (make-tile (tile-x p)
             (tile-y p)))

(define (fn-for-lop lop)
  (cons (fn-for-tile (first lop))
        (fn-for-lop (rest lop))))

(@htdd Snek)
(define-struct snek (lop dir))
;; Snek is (make-snek (listof tile) Dir)
;; interp. The Snek made of a list of tiles and a direction it's heading in

(define SNEK1 (make-snek (list S1) D1))
(define SNEK2 (make-snek (list S2) D1))
(define SNEK3 (make-snek (list S1 S2) D1))

(define (fn-for-snek s)
  (make-snek (fn-for-lop (snek-lop s))
             (fn-for-dir (snek-dir s))))



(@htdd Game)
(define-struct game (snek food))
;; Game is (make-game Snek tile)
;; interp. The status of the current game with the snek and a tile of food

(define G1 (make-game SNEK1 F1))
(define G2 (make-game SNEK3 F1))

(define (fn-for-game g)
  (make-game (fn-for-snek (game-snek g))
             (fn-for-tile (game-food g))))


;; =================
;; Functions:

(@htdf main)
(@signature Game -> Game)
;; start the world with (Game G1)
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
                 (next-food (game-food g))
                 (game-food g)))))
                  

(@htdf next-snek)
(@signature Snek Boolean -> Snek)
;; Produce the next Snek by moving each of its tiles
(check-expect (next-snek SNEK1 false) SNEK2)

(define (next-snek s ef)
  (make-snek (cond [(empty? (snek-lop s)) (snek-lop s)]
                   [else (cons (next-head (first (snek-lop s)) (snek-dir s))
                               (if ef
                                   (snek-lop s)
                                   (next-body (rest (snek-lop s))
                                          (first (snek-lop s)))))])
             (snek-dir s)))

(@htdf next-head)
(@signature Tile Dir -> Tile)
;; produce the next head of the Snek by moving it in the current direction
(check-expect (next-head S1 "R") (make-tile 12 11))
(check-expect (next-head (make-tile 12 11) "L") (make-tile 11 11))
(check-expect (next-head (make-tile 11 11) "U") (make-tile 11 10))
(check-expect (next-head (make-tile 11 10) "D") (make-tile 11 11))
(check-expect (next-head (make-tile 1 11) "L") (make-tile 1 11))
(check-expect (next-head (make-tile 2 11) "L") (make-tile 1 11))
(check-expect (next-head (make-tile SQUARES-PER-SIDE 11) "R")
              (make-tile SQUARES-PER-SIDE 11))
(check-expect (next-head (make-tile (- SQUARES-PER-SIDE 1) 11) "R")
              (make-tile SQUARES-PER-SIDE 11))
(check-expect (next-head (make-tile 11 1) "U") (make-tile 11 1))
(check-expect (next-head (make-tile 11 2) "U") (make-tile 11 1))
(check-expect (next-head (make-tile 11 SQUARES-PER-SIDE) "D")
              (make-tile 11 SQUARES-PER-SIDE))
(check-expect (next-head (make-tile 11 (- SQUARES-PER-SIDE 1)) "D")
              (make-tile 11 SQUARES-PER-SIDE))
    
(define (next-head p d)
  (local [(define (move-left p)
            (if (<= (tile-x p) 1)
                (make-tile 1 (tile-y p))
                (make-tile (- (tile-x p) 1) (tile-y p))))

          (define (move-right p)
            (if (>= (tile-x p) SQUARES-PER-SIDE)
                (make-tile SQUARES-PER-SIDE (tile-y p))
                (make-tile (+ (tile-x p) 1) (tile-y p))))

          (define (move-up p)
            (if (<= (tile-y p) 1)
                (make-tile (tile-x p) 1)
                (make-tile (tile-x p) (- (tile-y p) 1))))

          (define (move-down p)
            (if (>= (tile-y p) SQUARES-PER-SIDE)
                (make-tile (tile-x p) SQUARES-PER-SIDE)
                (make-tile (tile-x p) (+ (tile-y p) 1))))]
    (cond [(string=? "U" d) (move-up p)]
          [(string=? "D" d) (move-down p)]
          [(string=? "L" d) (move-left p)]
          [else (move-right p)]))) 


(@htdf next-body)
(@signature (listof Tile) Tile -> (listof Tile))
;; Produce next body of snake by moving each tile to follow the next one
(check-expect (next-body empty S1) empty)
(check-expect (next-body (list S2) S1) (list S1)) 

(define (next-body lop lp)
  (cond [(empty? lop) empty]
        [else
         (cons lp
               (next-body (rest lop)
                          (first lop)))]))

(@htdf next-food)
(@signature Tile -> Tile)
;; Produce the next food pos by moving it if it has been eaten by head of snek
(check-random (next-food F1)
              (make-tile (+ (random (- SQUARES-PER-SIDE 1)) 1)
                         (+ (random (- SQUARES-PER-SIDE 1)) 1)))

(@template Tile) 
(define (next-food f)
  (make-tile (+ (random (- SQUARES-PER-SIDE 1)) 1)
             (+ (random (- SQUARES-PER-SIDE 1)) 1)))

(@htdf render-game)
(@signature Game -> Image)
;; Render the game including the snek head and food 
(check-expect (render-game G1)
              (place-image TILE
                           (- (* 16 SQUARE-SIZE) (/ SQUARE-SIZE 2))
                           (- (* 11 SQUARE-SIZE) (/ SQUARE-SIZE 2))
                           (place-image TILE
                                        (- (* 11 SQUARE-SIZE) (/ SQUARE-SIZE 2))
                                        (- (* 11 SQUARE-SIZE) (/ SQUARE-SIZE 2))
                                        MTS)))

(@template Game)
(define (render-game g)
  (render-lot (cons (game-food g) (snek-lop (game-snek g)))))

(@htdf render-lot)
(@signature (listof Tile) -> Image)
;; Render a list of tiles onto the scene

(define (render-lot lot)
  (cond [(empty? lot) MTS]
        [else
         (local [(define (tile-pos n)
                   (- (* n SQUARE-SIZE) (/ SQUARE-SIZE 2)))]
           (place-image TILE
                        (tile-pos (tile-x (first lot)))
                        (tile-pos (tile-y (first lot)))
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
  
