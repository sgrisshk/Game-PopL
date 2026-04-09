#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;; For now this file is only the data core:
;; no movement, no rendering, no on-key logic yet.


;; Tile


;; Tile constants.
;; This is easier than hardcoding strings everywhere.
(define WALL "Wall")
(define FLOOR "Floor")
(define KEY "Key")
(define DOOR "Door")
(define EXIT "Exit")

;; A Tile is one of:
;; - WALL
;; - FLOOR
;; - KEY
;; - DOOR
;; - EXIT
;; interpretation: what is stored in one map cell.
;; Example meaning: wall, floor, key, etc.

(define TILE-1 WALL)
(define TILE-2 KEY)

;; Tile -> ...
;; template for functions consuming Tile
;; Template idea: one question per Tile option.
(define (tile-template t)
  (cond
    [(string=? t WALL) ...]
    [(string=? t FLOOR) ...]
    [(string=? t KEY) ...]
    [(string=? t DOOR) ...]
    [(string=? t EXIT) ...]))



;; Pos


;; Regular coordinate struct (x, y).
(define-struct pos [x y])
;; A Pos is (make-pos Integer Integer)
;; interpretation: coordinate on the grid,
;; x = column index, y = row index.

(define POS-1 (make-pos 0 0))
(define POS-2 (make-pos 3 2))

;; Pos -> ...
;; template for functions consuming Pos
;; Template: extract both fields from the struct.
(define (pos-template p)
  (... (pos-x p)
       (pos-y p)))



;; Player


;; Player data: where the player stands and how many keys are collected.
(define-struct player [pos keys])
;; A Player is (make-player Pos Natural)
;; interpretation: current player state:
;; position + number of collected keys.

(define PLAYER-1 (make-player (make-pos 1 1) 0))
(define PLAYER-2 (make-player (make-pos 4 2) 2))

;; Player -> ...
;; template for functions consuming Player
;; First unpack player, then process nested position.
(define (player-template p)
  (... (pos-template (player-pos p))
       (player-keys p)))



;; Grid


;; A Grid is [List-of [List-of Tile]]
;; interpretation: full dungeon map.
;; Outer list = all rows, inner list = one row.
;; So basically this is a matrix of Tile values.

(define GRID-1
  (list
   (list WALL WALL WALL)
   (list WALL FLOOR WALL)
   (list WALL EXIT WALL)))

(define GRID-2
  (list
   (list WALL WALL WALL WALL)
   (list WALL KEY FLOOR WALL)
   (list WALL DOOR EXIT WALL)
   (list WALL WALL WALL WALL)))

;; Grid -> ...
;; template for functions consuming Grid
;; Template for the list of rows.
(define (grid-template g)
  (cond
    [(empty? g) ...]
    [else
     (... (row-template (first g))
          (grid-template (rest g)))]))

;; [List-of Tile] -> ...
;; helper template for one grid row
;; Template for one row (a list of cells).
(define (row-template row)
  (cond
    [(empty? row) ...]
    [else
     (... (tile-template (first row))
          (row-template (rest row)))]))



;; Status


(define PLAYING "playing")
(define WON "won")
(define LOST "lost")

;; A Status is one of:
;; - PLAYING
;; - WON
;; - LOST
;; interpretation: global game status.
;; playing = running, won = victory, lost = defeat.

(define STATUS-1 PLAYING)
(define STATUS-2 WON)

;; Status -> ...
;; template for functions consuming Status
;; Same one-of template style as Tile.
(define (status-template s)
  (cond
    [(string=? s PLAYING) ...]
    [(string=? s WON) ...]
    [(string=? s LOST) ...]))



;; World


;; Full snapshot of game state.
(define-struct world [grid player status])
;; A World is (make-world Grid Player Status)
;; interpretation: complete (immutable) state:
;; map, player, and current game status.

(define WORLD-1
  (make-world GRID-1 PLAYER-1 PLAYING))

(define WORLD-2
  (make-world GRID-2 PLAYER-2 WON))

;; World -> ...
;; template for functions consuming World
;; Basic top-level template for world functions.
(define (world-template w)
  (... (grid-template (world-grid w))
       (player-template (world-player w))
       (status-template (world-status w))))



;; Movement


;; Movement test data:
;; The player starts at (1,1), can move right to floor, and cannot move into walls.
(define MOVE-TEST-GRID
  (list
   (list WALL WALL WALL)
   (list WALL FLOOR FLOOR)
   (list WALL WALL WALL)))

(define MOVE-TEST-WORLD
  (make-world MOVE-TEST-GRID
              (make-player (make-pos 1 1) 0)
              PLAYING))

(define MOVE-TEST-WORLD-RIGHT
  (make-world MOVE-TEST-GRID
              (make-player (make-pos 2 1) 0)
              PLAYING))


;; movement-key? : KeyEvent -> Boolean
;; purpose: check if a key event is one of the 4 movement arrows.
;; examples:
;; (movement-key? "left")  => #true
;; (movement-key? "a")     => #false
(check-expect (movement-key? "left") #true)
(check-expect (movement-key? "up") #true)
(check-expect (movement-key? "a") #false)

(define (movement-key? ke)
  (or (string=? ke "up")
      (string=? ke "down")
      (string=? ke "left")
      (string=? ke "right")))


;; next-pos : Pos KeyEvent -> Pos
;; purpose: produce the next position from a position and arrow key.
;; If the key is not an arrow key, return the same position.
;; examples:
;; (next-pos (make-pos 2 2) "up")    => (make-pos 2 1)
;; (next-pos (make-pos 2 2) "right") => (make-pos 3 2)
(check-expect (next-pos (make-pos 2 2) "up") (make-pos 2 1))
(check-expect (next-pos (make-pos 2 2) "down") (make-pos 2 3))
(check-expect (next-pos (make-pos 2 2) "left") (make-pos 1 2))
(check-expect (next-pos (make-pos 2 2) "right") (make-pos 3 2))
(check-expect (next-pos (make-pos 2 2) "x") (make-pos 2 2))

(define (next-pos p ke)
  (cond
    [(string=? ke "up") (make-pos (pos-x p) (sub1 (pos-y p)))]
    [(string=? ke "down") (make-pos (pos-x p) (add1 (pos-y p)))]
    [(string=? ke "left") (make-pos (sub1 (pos-x p)) (pos-y p))]
    [(string=? ke "right") (make-pos (add1 (pos-x p)) (pos-y p))]
    [else p]))


;; in-grid? : Grid Pos -> Boolean
;; purpose: check whether a position is inside grid bounds.
;; examples:
;; (in-grid? MOVE-TEST-GRID (make-pos 2 1)) => #true
;; (in-grid? MOVE-TEST-GRID (make-pos 3 1)) => #false
(check-expect (in-grid? MOVE-TEST-GRID (make-pos 2 1)) #true)
(check-expect (in-grid? MOVE-TEST-GRID (make-pos 3 1)) #false)
(check-expect (in-grid? MOVE-TEST-GRID (make-pos -1 1)) #false)

(define (in-grid? g p)
  (and (not (empty? g))
       (<= 0 (pos-x p))
       (<= 0 (pos-y p))
       (< (pos-y p) (length g))
       (< (pos-x p) (length (first g)))))


;; nth-item : [NEList-of X] Natural -> X
;; purpose: return the n-th item (0-based) from a non-empty list.
;; examples:
;; (nth-item (list "a" "b" "c") 0) => "a"
;; (nth-item (list "a" "b" "c") 2) => "c"
(check-expect (nth-item (list "a" "b" "c") 0) "a")
(check-expect (nth-item (list "a" "b" "c") 2) "c")

(define (nth-item l n)
  (cond
    [(zero? n) (first l)]
    [else (nth-item (rest l) (sub1 n))]))


;; tile-at : Grid Pos -> Tile
;; purpose: get the tile value at a position.
;; examples:
;; (tile-at MOVE-TEST-GRID (make-pos 1 1)) => FLOOR
;; (tile-at MOVE-TEST-GRID (make-pos 0 1)) => WALL
(check-expect (tile-at MOVE-TEST-GRID (make-pos 1 1)) FLOOR)
(check-expect (tile-at MOVE-TEST-GRID (make-pos 0 1)) WALL)

(define (tile-at g p)
  (nth-item (nth-item g (pos-y p)) (pos-x p)))


;; valid-move? : World Pos -> Boolean
;; purpose: check if a move target is inside the grid and not a wall tile.
;; examples:
;; (valid-move? MOVE-TEST-WORLD (make-pos 2 1)) => #true
;; (valid-move? MOVE-TEST-WORLD (make-pos 0 1)) => #false
(check-expect (valid-move? MOVE-TEST-WORLD (make-pos 2 1)) #true)
(check-expect (valid-move? MOVE-TEST-WORLD (make-pos 0 1)) #false)
(check-expect (valid-move? MOVE-TEST-WORLD (make-pos 3 1)) #false)

(define (valid-move? w p)
  (and (in-grid? (world-grid w) p)
       (not (string=? (tile-at (world-grid w) p) WALL))))


;; move-player : World KeyEvent -> World
;; purpose: move the player by arrow key if target tile is valid.
;; If the key is not an arrow key or target is invalid, return unchanged world.
;; examples:
;; (move-player MOVE-TEST-WORLD "right") => MOVE-TEST-WORLD-RIGHT
;; (move-player MOVE-TEST-WORLD "left")  => MOVE-TEST-WORLD
(check-expect (move-player MOVE-TEST-WORLD "right") MOVE-TEST-WORLD-RIGHT)
(check-expect (move-player MOVE-TEST-WORLD "left") MOVE-TEST-WORLD)
(check-expect (move-player MOVE-TEST-WORLD "up") MOVE-TEST-WORLD)
(check-expect (move-player MOVE-TEST-WORLD "a") MOVE-TEST-WORLD)

(define (move-player w ke)
  (cond
    [(not (movement-key? ke)) w]
    [else
     (local [(define old-player (world-player w))
             (define target-pos (next-pos (player-pos old-player) ke))]
       (if (valid-move? w target-pos)
           (make-world (world-grid w)
                       (make-player target-pos
                                    (player-keys old-player))
                       (world-status w))
           w))]))
