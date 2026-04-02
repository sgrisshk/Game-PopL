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
;; A Pos is (make-pos Natural Natural)
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

(define WORLD-EXAMPLE-1
  (make-world GRID-1 PLAYER-1 PLAYING))

(define WORLD-EXAMPLE-2
  (make-world GRID-2 PLAYER-2 WON))

;; World -> ...
;; template for functions consuming World
;; Basic top-level template for world functions.
(define (world-template w)
  (... (grid-template (world-grid w))
       (player-template (world-player w))
       (status-template (world-status w))))
