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
(define PIECE (square SQUARE-SIZE "solid" "white"))

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

(@htdd Piece)
(define-struct piece (x y))
;; Piece is (make-piece Natural Natural)
;; interp. The x y position of a piece on the scene
;; CONSTRAINT: piece-x and piece-y are in (0,SQUARES-PER-SIDE)

(define S1 (make-piece 11 11))
(define S2 (make-piece 12 11))
(define F1 (make-piece 16 11))

(define (fn-for-piece p)
  (make-piece (piece-x p)
              (piece-y p)))

(define (fn-for-lop lop)
  (cons (fn-for-piece (first lop))
        (fn-for-lop (rest lop))))

(@htdd Snek)
(define-struct snek (lop dir))
;; Snek is (make-snek (listof Piece) Dir)
;; interp. The Snek made of a list of pieces and a direction it's heading in

(define SNEK1 (make-snek (list S1) D1))
(define SNEK2 (make-snek (list S2) D1))

(define (fn-for-snek s)
  (make-snek (fn-for-lop (snek-lop s))
             (fn-for-dir (snek-dir s))))



(@htdd Game)
(define-struct game (snek food))
;; Game is (make-game Snek Piece)
;; interp. The status of the current game with the snek and a piece of food

(define G1 (make-game SNEK1 F1))

(define (fn-for-game g)
  (make-game (fn-for-snek (game-snek g))
             (fn-for-piece (game-food g))))


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
  (local [(define ns (next-snek (game-snek g)))]
    (make-game ns (next-food (game-food g) ns))))
                  

(@htdf next-snek)
(@signature Snek -> Snek)
;; Produce the next listof piece by moving each of them one square
(check-expect (next-snek SNEK1) SNEK2)

(define (next-snek s)
  (local [(define (next-lop-body lop-body lp)
            (cond [(empty? lop-body) empty]
                  [else
                   (cons (make-piece (piece-x lp) (piece-y lp))
                         (next-lop-body (rest lop-body)
                                        (first lop-body)))]))]
    (make-snek (cond [(empty? (snek-lop s)) (snek-lop s)]
                     [else (cons (next-head (first (snek-lop s)) (snek-dir s))
                                 (next-lop-body (rest (snek-lop s))
                                                (first (snek-lop s))))])
               (snek-dir s))))
          

(@htdf next-head)
(@signature Piece Dir -> Piece)
;; produce the head of the Snek by moving it in the current direction
(check-expect (next-head S1 "R") (make-piece 12 11))
(check-expect (next-head (make-piece 12 11) "L") (make-piece 11 11))
(check-expect (next-head (make-piece 11 11) "U") (make-piece 11 10))
(check-expect (next-head (make-piece 11 10) "D") (make-piece 11 11))
(check-expect (next-head (make-piece 1 11) "L") (make-piece 1 11))
(check-expect (next-head (make-piece 2 11) "L") (make-piece 1 11))
(check-expect (next-head (make-piece SQUARES-PER-SIDE 11) "R")
              (make-piece SQUARES-PER-SIDE 11))
(check-expect (next-head (make-piece (- SQUARES-PER-SIDE 1) 11) "R")
              (make-piece SQUARES-PER-SIDE 11))
(check-expect (next-head (make-piece 11 1) "U") (make-piece 11 1))
(check-expect (next-head (make-piece 11 2) "U") (make-piece 11 1))
(check-expect (next-head (make-piece 11 SQUARES-PER-SIDE) "D")
              (make-piece 11 SQUARES-PER-SIDE))
(check-expect (next-head (make-piece 11 (- SQUARES-PER-SIDE 1)) "D")
              (make-piece 11 SQUARES-PER-SIDE))
    
(define (next-head p d)
  (local [(define (move-left p)
            (if (<= (piece-x p) 1)
                (make-piece 1 (piece-y p))
                (make-piece (- (piece-x p) 1) (piece-y p))))

          (define (move-right p)
            (if (>= (piece-x p) SQUARES-PER-SIDE)
                (make-piece SQUARES-PER-SIDE (piece-y p))
                (make-piece (+ (piece-x p) 1) (piece-y p))))

          (define (move-up p)
            (if (<= (piece-y p) 1)
                (make-piece (piece-x p) 1)
                (make-piece (piece-x p) (- (piece-y p) 1))))

          (define (move-down p)
            (if (>= (piece-y p) SQUARES-PER-SIDE)
                (make-piece (piece-x p) SQUARES-PER-SIDE)
                (make-piece (piece-x p) (+ (piece-y p) 1))))]
    (cond [(string=? "U" d) (move-up p)]
          [(string=? "D" d) (move-down p)]
          [(string=? "L" d) (move-left p)]
          [else (move-right p)])))

(@htdf next-food)
(@signature Piece Snek -> Piece)
;; Produce the next food pos by moving it if it has been eaten by head of snek
(check-expect (next-food F1 SNEK1) F1)
(check-random (next-food F1 (make-snek (list (make-piece 16 11)) "R"))
              (make-piece (random SQUARES-PER-SIDE) (random SQUARES-PER-SIDE)))

(@template Piece) 
(define (next-food f s)
  (local [(define (eat? f s)
            (and (= (piece-x f) (piece-x s)) (= (piece-y f) (piece-y s))))]
    (if (eat? f (first (snek-lop s)))
        (make-piece (random SQUARES-PER-SIDE) (random SQUARES-PER-SIDE))
        f)))

(@htdf render-game)
(@signature Game -> Image)
;; Render the game including the snek head and food
(check-expect (render-game G1)
              (place-image PIECE
                           (- (* 16 SQUARE-SIZE) (/ SQUARE-SIZE 2))
                           (- (* 11 SQUARE-SIZE) (/ SQUARE-SIZE 2))
                           (place-image PIECE
                                        (- (* 11 SQUARE-SIZE) (/ SQUARE-SIZE 2))
                                        (- (* 11 SQUARE-SIZE) (/ SQUARE-SIZE 2))
                                        MTS)))

(@template fn-composition)
(define (render-game g)
  (render-piece (game-food g)
                (render-piece (first (snek-lop (game-snek g)))
                              MTS))) 

(@htdf render-piece)
(@signature Piece Image -> Image)
;; render the head of the snek on to the scene
(check-expect (render-piece (make-piece 1 1) MTS) 
              (place-image PIECE
                           (/ SQUARE-SIZE 2)
                           (/ SQUARE-SIZE 2)
                           MTS))
(check-expect (render-piece (make-piece 1 11) MTS)
              (place-image PIECE
                           (/ SQUARE-SIZE 2)
                           (- (* 11 SQUARE-SIZE) (/ SQUARE-SIZE 2))
                           MTS))
(check-expect (render-piece (make-piece 11 1) MTS)
              (place-image PIECE
                           (- (* 11 SQUARE-SIZE) (/ SQUARE-SIZE 2))
                           (/ SQUARE-SIZE 2)
                           MTS))
(check-expect (render-piece (make-piece 11 11) MTS)
              (place-image PIECE
                           (- (* 11 SQUARE-SIZE) (/ SQUARE-SIZE 2))
                           (- (* 11 SQUARE-SIZE) (/ SQUARE-SIZE 2))
                           MTS))
(check-expect (render-piece (make-piece SQUARES-PER-SIDE SQUARES-PER-SIDE)
                            MTS)
              (place-image PIECE
                           (- (* SQUARES-PER-SIDE SQUARE-SIZE)
                              (/ SQUARE-SIZE 2))
                           (- (* SQUARES-PER-SIDE SQUARE-SIZE)
                              (/ SQUARE-SIZE 2))
                           MTS))


(define (render-piece p i)
  (place-image PIECE
               (piece-pos (piece-x p))
               (piece-pos (piece-y p))
               i))

(define (piece-pos n)
  (- (* n SQUARE-SIZE) (/ SQUARE-SIZE 2)))


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
  
