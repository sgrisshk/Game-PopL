#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;;------ TUTORIAL 1

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



;; Enemy


;; Enemy data: where one enemy stands.
(define-struct enemy [pos])
;; An Enemy is (make-enemy Pos)
;; interpretation: one hostile actor on the grid.
;; If an enemy has the same position as the player, the player loses.

(define ENEMY-1 (make-enemy (make-pos 2 1)))
(define ENEMY-2 (make-enemy (make-pos 2 2)))

;; Enemy -> ...
;; template for functions consuming Enemy
;; FP note: enemy state is immutable, so movement returns a new Enemy.
(define (enemy-template e)
  (... (pos-template (enemy-pos e))))

;; [List-of Enemy] -> ...
;; template for functions consuming a list of enemies
;; FP note: structural recursion processes one enemy and recurs on the rest.
(define (enemies-template enemies)
  (cond
    [(empty? enemies) ...]
    [else
     (... (enemy-template (first enemies))
          (enemies-template (rest enemies)))]))



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
(define-struct world [grid player enemies status])
;; A World is (make-world Grid Player [List-of Enemy] Status)
;; interpretation: complete (immutable) state:
;; map, player, enemies, and current game status.

(define WORLD-1
  (make-world GRID-1 PLAYER-1 empty PLAYING))

(define WORLD-2
  (make-world GRID-2 PLAYER-2 (list ENEMY-1 ENEMY-2) WON))

;; World -> ...
;; template for functions consuming World
;; Basic top-level template for world functions.
;; FP note: a World value is immutable.
;; Every game step returns a NEW World instead of modifying old state.
(define (world-template w)
  (... (grid-template (world-grid w))
       (player-template (world-player w))
       (enemies-template (world-enemies w))
       (status-template (world-status w))))

;;------ TUTORIAL 2

;; Movement


;; Movement test data:
;; The player starts at (1,1), can move right to floor, and cannot move into walls.
(define MOVE-GRID
  (list
   (list WALL WALL WALL)
   (list WALL FLOOR FLOOR)
   (list WALL WALL WALL)))

(define MOVE-WORLD
  (make-world MOVE-GRID
              (make-player (make-pos 1 1) 0)
              empty
              PLAYING))

(define MOVE-WORLD-RIGHT
  (make-world MOVE-GRID
              (make-player (make-pos 2 1) 0)
              empty
              PLAYING))


;; movement-key? : KeyEvent -> Boolean
;; purpose: check if a key event is one of the 4 movement arrows.
;; FP note: pure predicate, no state changes.
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
;; FP note: this is a small deterministic transformation Pos -> Pos.
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
;; FP note: read-only function over Grid and Pos.
;; examples:
;; (in-grid? MOVE-GRID(make-pos 2 1)) => #true
;; (in-grid? MOVE-GRID(make-pos 3 1)) => #false
(check-expect (in-grid? MOVE-GRID(make-pos 2 1)) #true)
(check-expect (in-grid? MOVE-GRID(make-pos 3 1)) #false)
(check-expect (in-grid? MOVE-GRID(make-pos -1 1)) #false)

(define (in-grid? g p)
  (and (not (empty? g))
       (<= 0 (pos-x p))
       (<= 0 (pos-y p))
       (< (pos-y p) (length g))
       (< (pos-x p) (length (first g)))))


;; nth-item : [NEList-of X] Natural -> X
;; purpose: return the n-th item (0-based) from a non-empty list.
;; FP note: structural recursion on lists.
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
;; FP note: composition of pure helpers (nth-item + field accessors).
;; examples:
;; (tile-at MOVE-GRID(make-pos 1 1)) => FLOOR
;; (tile-at MOVE-GRID(make-pos 0 1)) => WALL
(check-expect (tile-at MOVE-GRID(make-pos 1 1)) FLOOR)
(check-expect (tile-at MOVE-GRID(make-pos 0 1)) WALL)

(define (tile-at g p)
  (nth-item (nth-item g (pos-y p)) (pos-x p)))


;; valid-move? : World Pos -> Boolean
;; purpose: check if a move target is inside the grid and not a wall tile.
;; FP note: this function only checks constraints, no mutation.
;; examples:
;; (valid-move? MOVE-WORLD (make-pos 2 1)) => #true
;; (valid-move? MOVE-WORLD (make-pos 0 1)) => #false
(check-expect (valid-move? MOVE-WORLD (make-pos 2 1)) #true)
(check-expect (valid-move? MOVE-WORLD (make-pos 0 1)) #false)
(check-expect (valid-move? MOVE-WORLD (make-pos 3 1)) #false)

;; passable-tile? : Player Tile -> Boolean
;; purpose: decide if player can stand on a tile with current key count.
;; FP note: business rule as a small pure decision function.
(define (passable-tile? p t)
  (cond
    [(string=? t WALL) #false]
    [(string=? t DOOR) (>= (player-keys p) 1)]
    [else #true]))

(define (valid-move? w p)
  (and (in-grid? (world-grid w) p)
       (passable-tile? (world-player w)
                       (tile-at (world-grid w) p))))


;; move-player : World KeyEvent -> World
;; purpose: move the player by arrow key if target tile is valid.
;; If the key is not an arrow key or target is invalid, return unchanged world.
;; FP note: input world is never changed; result is either old world
;; or a newly constructed world with updated player position.
;; examples:
;; (move-player MOVE-WORLD "right") => MOVE-WORLD-RIGHT
;; (move-player MOVE-WORLD "left")  => MOVE-WORLD
(check-expect (move-player MOVE-WORLD "right") MOVE-WORLD-RIGHT)
(check-expect (move-player MOVE-WORLD "left") MOVE-WORLD)
(check-expect (move-player MOVE-WORLD "up") MOVE-WORLD)
(check-expect (move-player MOVE-WORLD "a") MOVE-WORLD)

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
                       (world-enemies w)
                       (world-status w))
           w))]))

;;------ TUTORIAL 3

;; Interaction

;; replace-at : [NEList-of X] Natural X -> [NEList-of X]
;; purpose: create a new list where index n is replaced with item.
;; FP note: this is immutable list update.
;; We rebuild only the needed prefix, then share tail structure.
;; examples:
;; (replace-at (list "a" "b" "c") 1 "x") => (list "a" "x" "c")
;; (replace-at (list 10 20 30) 0 5) => (list 5 20 30)
(check-expect (replace-at (list "a" "b" "c") 1 "x")
              (list "a" "x" "c"))
(check-expect (replace-at (list 10 20 30) 0 5)
              (list 5 20 30))

(define (replace-at l n item)
  (cond
    [(zero? n) (cons item (rest l))]
    [else (cons (first l)
                (replace-at (rest l) (sub1 n) item))]))


;; set-tile : Grid Pos Tile -> Grid
;; purpose: produce a new grid where tile at position p is replaced with tile.
;; FP note: two-level immutable update:
;; update the row, update the grid with that new row.
;; examples:
;; (tile-at (set-tile MOVE-GRID (make-pos 1 1) KEY) (make-pos 1 1)) => KEY
;; (tile-at (set-tile MOVE-GRID (make-pos 2 1) WALL) (make-pos 2 1)) => WALL
(check-expect (tile-at (set-tile MOVE-GRID (make-pos 1 1) KEY)
                       (make-pos 1 1))
              KEY)
(check-expect (tile-at (set-tile MOVE-GRID (make-pos 2 1) WALL)
                       (make-pos 2 1))
              WALL)

(define (set-tile g p tile)
  (replace-at g
              (pos-y p)
              (replace-at (nth-item g (pos-y p))
                          (pos-x p)
                          tile)))


;; Interaction test data:
(define INTERACT-GRID
  (list
   (list FLOOR KEY DOOR EXIT)))

(define INTERACT-WORLD-FLOOR
  (make-world INTERACT-GRID
              (make-player (make-pos 0 0) 0)
              empty
              PLAYING))

(define INTERACT-WORLD-KEY
  (make-world INTERACT-GRID
              (make-player (make-pos 1 0) 0)
              empty
              PLAYING))

(define INTERACT-WORLD-DOOR-WITH-KEY
  (make-world INTERACT-GRID
              (make-player (make-pos 2 0) 2)
              empty
              PLAYING))

(define INTERACT-WORLD-DOOR-NO-KEY
  (make-world INTERACT-GRID
              (make-player (make-pos 2 0) 0)
              empty
              PLAYING))

(define INTERACT-WORLD-EXIT
  (make-world INTERACT-GRID
              (make-player (make-pos 3 0) 0)
              empty
              PLAYING))


;; handle-tile : World -> World
;; purpose: apply tile interaction at player's current position.
;; KEY: add one key and replace tile with FLOOR.
;; DOOR: consume one key if player has one, otherwise unchanged.
;; EXIT: set world status to WON.
;; FLOOR: unchanged.
;; FP note: tile interaction is expressed as case analysis over Tile,
;; and each case returns a brand-new World value.
;; examples:
;; (handle-tile INTERACT-WORLD-FLOOR) => INTERACT-WORLD-FLOOR
;; (handle-tile INTERACT-WORLD-EXIT)  => world with WON status
(check-expect (handle-tile INTERACT-WORLD-FLOOR)
              INTERACT-WORLD-FLOOR)

(check-expect (handle-tile INTERACT-WORLD-KEY)
              (make-world (list (list FLOOR FLOOR DOOR EXIT))
                          (make-player (make-pos 1 0) 1)
                          empty
                          PLAYING))

(check-expect (handle-tile INTERACT-WORLD-DOOR-WITH-KEY)
              (make-world INTERACT-GRID
                          (make-player (make-pos 2 0) 1)
                          empty
                          PLAYING))

(check-expect (handle-tile INTERACT-WORLD-DOOR-NO-KEY)
              INTERACT-WORLD-DOOR-NO-KEY)

(check-expect (handle-tile INTERACT-WORLD-EXIT)
              (make-world INTERACT-GRID
                          (make-player (make-pos 3 0) 0)
                          empty
                          WON))

(define (handle-tile w)
  (local [(define p (world-player w))
          (define p-pos (player-pos p))
          (define current (tile-at (world-grid w) p-pos))]
    (cond
      [(string=? current KEY)
       (make-world (set-tile (world-grid w) p-pos FLOOR)
                   (make-player p-pos (add1 (player-keys p)))
                   (world-enemies w)
                   (world-status w))]
      [(string=? current DOOR)
       (if (>= (player-keys p) 1)
           (make-world (world-grid w)
                       (make-player p-pos (sub1 (player-keys p)))
                       (world-enemies w)
                       (world-status w))
           w)]
      [(string=? current EXIT)
       (make-world (world-grid w)
                   p
                   (world-enemies w)
                   WON)]
      [else w])))

;;------ TUTORIAL 4

;; Enemies

;; Enemy movement test data:
;; The enemy pattern is deterministic:
;; try to move right first; if blocked, try left; if blocked again, stay.
;; FP note: deterministic movement keeps tests pure and reproducible.
(define ENEMY-GRID
  (list
   (list WALL WALL WALL WALL)
   (list WALL FLOOR FLOOR WALL)
   (list WALL FLOOR WALL WALL)))

(define ENEMY-WORLD-MOVE
  (make-world ENEMY-GRID
              (make-player (make-pos 1 2) 0)
              (list (make-enemy (make-pos 1 1)))
              PLAYING))

(define ENEMY-WORLD-MOVED
  (make-world ENEMY-GRID
              (make-player (make-pos 1 2) 0)
              (list (make-enemy (make-pos 2 1)))
              PLAYING))

(define ENEMY-WORLD-WALL
  (make-world ENEMY-GRID
              (make-player (make-pos 2 1) 0)
              (list (make-enemy (make-pos 1 2)))
              PLAYING))

(define ENEMY-WORLD-WALL-STAY
  (make-world ENEMY-GRID
              (make-player (make-pos 2 1) 0)
              (list (make-enemy (make-pos 1 2)))
              PLAYING))

(define ENEMY-WORLD-COLLISION
  (make-world ENEMY-GRID
              (make-player (make-pos 2 1) 0)
              (list (make-enemy (make-pos 1 1)))
              PLAYING))

(define ENEMY-WORLD-COLLISION-LOST
  (make-world ENEMY-GRID
              (make-player (make-pos 2 1) 0)
              (list (make-enemy (make-pos 2 1)))
              LOST))

(define ENEMY-WORLD-START-COLLISION
  (make-world ENEMY-GRID
              (make-player (make-pos 1 1) 0)
              (list (make-enemy (make-pos 1 1)))
              PLAYING))

(define ENEMY-WORLD-START-COLLISION-LOST
  (make-world ENEMY-GRID
              (make-player (make-pos 1 1) 0)
              (list (make-enemy (make-pos 1 1)))
              LOST))


;; pos=? : Pos Pos -> Boolean
;; purpose: decide whether two positions have the same coordinates.
;; FP note: pure equality over immutable structs.
(check-expect (pos=? (make-pos 1 2) (make-pos 1 2)) #true)
(check-expect (pos=? (make-pos 1 2) (make-pos 2 1)) #false)

(define (pos=? p1 p2)
  (and (= (pos-x p1) (pos-x p2))
       (= (pos-y p1) (pos-y p2))))


;; enemy-valid-move? : Grid Pos -> Boolean
;; purpose: check if an enemy may occupy a position.
;; Rule: enemies must stay inside the grid and cannot move through WALL.
;; FP note: this function reads Grid and Pos only; it has no side effects.
(check-expect (enemy-valid-move? ENEMY-GRID (make-pos 2 1)) #true)
(check-expect (enemy-valid-move? ENEMY-GRID (make-pos 3 1)) #false)
(check-expect (enemy-valid-move? ENEMY-GRID (make-pos -1 1)) #false)

(define (enemy-valid-move? g p)
  (and (in-grid? g p)
       (not (string=? (tile-at g p) WALL))))


;; move-enemy : Grid Enemy -> Enemy
;; purpose: move one enemy using a simple deterministic pattern.
;; Pattern: right if possible, otherwise left if possible, otherwise stay.
;; FP note: no Enemy is changed in place; this returns a new Enemy value.
(check-expect (move-enemy ENEMY-GRID (make-enemy (make-pos 1 1)))
              (make-enemy (make-pos 2 1)))
(check-expect (move-enemy ENEMY-GRID (make-enemy (make-pos 1 2)))
              (make-enemy (make-pos 1 2)))

(define (move-enemy g e)
  (local [(define current-pos (enemy-pos e))
          (define right-pos (next-pos current-pos "right"))
          (define left-pos (next-pos current-pos "left"))]
    (cond
      [(enemy-valid-move? g right-pos) (make-enemy right-pos)]
      [(enemy-valid-move? g left-pos) (make-enemy left-pos)]
      [else e])))


;; move-enemies : Grid [List-of Enemy] -> [List-of Enemy]
;; purpose: move every enemy in a list.
;; FP note: structural recursion builds a fresh list of fresh Enemy values.
(check-expect (move-enemies ENEMY-GRID
                            (list (make-enemy (make-pos 1 1))
                                  (make-enemy (make-pos 1 2))))
              (list (make-enemy (make-pos 2 1))
                    (make-enemy (make-pos 1 2))))

(define (move-enemies g enemies)
  (cond
    [(empty? enemies) empty]
    [else
     (cons (move-enemy g (first enemies))
           (move-enemies g (rest enemies)))]))


;; enemy-at? : Pos Enemy -> Boolean
;; purpose: decide whether one enemy occupies a position.
;; FP note: small pure predicate used by the list collision check.
(check-expect (enemy-at? (make-pos 2 1) (make-enemy (make-pos 2 1))) #true)
(check-expect (enemy-at? (make-pos 2 1) (make-enemy (make-pos 1 1))) #false)

(define (enemy-at? p e)
  (pos=? p (enemy-pos e)))


;; any-enemy-at? : Pos [List-of Enemy] -> Boolean
;; purpose: check whether any enemy occupies a position.
;; FP note: structural recursion over enemies; no mutation or early state update.
(check-expect (any-enemy-at? (make-pos 2 1)
                             (list (make-enemy (make-pos 1 1))
                                   (make-enemy (make-pos 2 1))))
              #true)
(check-expect (any-enemy-at? (make-pos 2 1)
                             (list (make-enemy (make-pos 1 1))))
              #false)

(define (any-enemy-at? p enemies)
  (cond
    [(empty? enemies) #false]
    [else
     (or (enemy-at? p (first enemies))
         (any-enemy-at? p (rest enemies)))]))


;; world-with-enemies : World [List-of Enemy] -> World
;; purpose: rebuild a world with a new enemy list and the same grid/player/status.
;; FP note: helper names the immutable update pattern for World.
(define (world-with-enemies w enemies)
  (make-world (world-grid w)
              (world-player w)
              enemies
              (world-status w)))


;; world-with-status : World Status -> World
;; purpose: rebuild a world with a new status and the same grid/player/enemies.
;; FP note: changing status means constructing a replacement World.
(define (world-with-status w status)
  (make-world (world-grid w)
              (world-player w)
              (world-enemies w)
              status))


;; collide-with-player? : World -> Boolean
;; purpose: check whether any enemy has the same position as the player.
;; FP note: this is a pure query over a complete World snapshot.
(check-expect (collide-with-player? ENEMY-WORLD-COLLISION-LOST) #true)
(check-expect (collide-with-player? ENEMY-WORLD-MOVE) #false)

(define (collide-with-player? w)
  (any-enemy-at? (player-pos (world-player w))
                 (world-enemies w)))


;; lose-if-collision : World -> World
;; purpose: set status to LOST if player and enemy occupy the same position.
;; FP note: returns either the original World or a rebuilt World with LOST status.
(check-expect (lose-if-collision ENEMY-WORLD-START-COLLISION)
              ENEMY-WORLD-START-COLLISION-LOST)
(check-expect (lose-if-collision ENEMY-WORLD-MOVE)
              ENEMY-WORLD-MOVE)

(define (lose-if-collision w)
  (if (collide-with-player? w)
      (world-with-status w LOST)
      w))


;; update-enemies : World -> World
;; purpose: move all enemies and apply enemy-player collision.
;; Rules:
;; - enemies move deterministically, right first then left;
;; - enemies stay within the grid;
;; - enemies cannot move through walls;
;; - if any enemy occupies the player's position, world status becomes LOST.
;; FP note: this function is a World -> World transformation.
;; It never mutates the grid, player, enemy list, or status in place.
(check-expect (update-enemies ENEMY-WORLD-MOVE)
              ENEMY-WORLD-MOVED)
(check-expect (update-enemies ENEMY-WORLD-WALL)
              ENEMY-WORLD-WALL-STAY)
(check-expect (update-enemies ENEMY-WORLD-COLLISION)
              ENEMY-WORLD-COLLISION-LOST)
(check-expect (update-enemies ENEMY-WORLD-START-COLLISION)
              ENEMY-WORLD-START-COLLISION-LOST)

(define (update-enemies w)
  (if (collide-with-player? w)
      (world-with-status w LOST)
      (lose-if-collision
       (world-with-enemies w
                           (move-enemies (world-grid w)
                                         (world-enemies w))))))

;;------ TUTORIAL 5

;; Rendering


(define TILE-SIZE 40)
(define HALF-TILE (/ TILE-SIZE 2))
(define ENTITY-RADIUS (quotient TILE-SIZE 3))

(define PLAYER-ICON (circle ENTITY-RADIUS "solid" "royal blue"))
(define ENEMY-ICON  (circle ENTITY-RADIUS "solid" "crimson"))


;; render-tile : Tile -> Image
;; purpose: produce a TILE-SIZE x TILE-SIZE image for one tile type.
;; FP note: pure; one call per tile, no state.
(check-expect (render-tile WALL)  (square TILE-SIZE "solid" "dark gray"))
(check-expect (render-tile FLOOR) (square TILE-SIZE "solid" "gainsboro"))

(define (render-tile t)
  (cond
    [(string=? t WALL)
     (square TILE-SIZE "solid" "dark gray")]
    [(string=? t FLOOR)
     (square TILE-SIZE "solid" "gainsboro")]
    [(string=? t KEY)
     (overlay (circle (quotient TILE-SIZE 4) "solid" "gold")
              (square TILE-SIZE "solid" "gainsboro"))]
    [(string=? t DOOR)
     (overlay (rectangle (quotient (* TILE-SIZE 3) 5)
                         (quotient (* TILE-SIZE 4) 5)
                         "solid" "saddlebrown")
              (square TILE-SIZE "solid" "peru"))]
    [(string=? t EXIT)
     (overlay (triangle (quotient TILE-SIZE 2) "solid" "white")
              (square TILE-SIZE "solid" "lime green"))]))


;; render-row : [List-of Tile] -> Image
;; purpose: join tile images horizontally into one row strip.
;; FP note: structural recursion over list; returns a new image per row.
(check-expect (render-row empty) empty-image)
(check-expect (render-row (list WALL))
              (beside (render-tile WALL) empty-image))

(define (render-row row)
  (cond
    [(empty? row) empty-image]
    [else (beside (render-tile (first row))
                  (render-row (rest row)))]))


;; render-grid : Grid -> Image
;; purpose: stack all row images vertically to form the dungeon background.
;; FP note: structural recursion over the outer list.
(check-expect (render-grid empty) empty-image)
(check-expect (render-grid (list (list WALL)))
              (above (render-row (list WALL)) empty-image))

(define (render-grid g)
  (cond
    [(empty? g) empty-image]
    [else (above (render-row (first g))
                 (render-grid (rest g)))]))


;; tile->pixel-center-x : Integer -> Number
;; purpose: pixel x-coordinate of the center of grid column col.
(check-expect (tile->pixel-center-x 0) HALF-TILE)
(check-expect (tile->pixel-center-x 2) (+ (* 2 TILE-SIZE) HALF-TILE))

(define (tile->pixel-center-x col)
  (+ (* col TILE-SIZE) HALF-TILE))


;; tile->pixel-center-y : Integer -> Number
;; purpose: pixel y-coordinate of the center of grid row row-idx.
(check-expect (tile->pixel-center-y 0) HALF-TILE)
(check-expect (tile->pixel-center-y 1) (+ TILE-SIZE HALF-TILE))

(define (tile->pixel-center-y row-idx)
  (+ (* row-idx TILE-SIZE) HALF-TILE))


;; place-at-pos : Image Pos Image -> Image
;; purpose: center icon on the tile at pos, drawn over bg.
;; FP note: place-image is a pure 2htdp/image function; no mutation.
(define (place-at-pos icon p bg)
  (place-image icon
               (tile->pixel-center-x (pos-x p))
               (tile->pixel-center-y (pos-y p))
               bg))


;; render-enemies : [List-of Enemy] Image -> Image
;; purpose: draw each enemy icon onto bg at its grid position.
;; FP note: structural recursion; each step returns a new image.
(check-expect (render-enemies empty (render-grid GRID-1))
              (render-grid GRID-1))

(define (render-enemies enemies bg)
  (cond
    [(empty? enemies) bg]
    [else
     (render-enemies (rest enemies)
                     (place-at-pos ENEMY-ICON
                                   (enemy-pos (first enemies))
                                   bg))]))


;; render-status : Status Image -> Image
;; purpose: overlay a message on img for WON or LOST; return img unchanged for PLAYING.
(check-expect (render-status PLAYING (square 10 "solid" "white"))
              (square 10 "solid" "white"))

(define (render-status s img)
  (cond
    [(string=? s WON)
     (overlay (text "YOU WIN!" 24 "white") img)]
    [(string=? s LOST)
     (overlay (text "GAME OVER" 24 "white") img)]
    [else img]))


;; render : World -> Image
;; purpose: convert a complete World snapshot into a single image.
;; Layers bottom to top: grid, enemies, player, status overlay.
;; FP note: functional shell; no mutation anywhere in the call tree.
(define (render w)
  (render-status
   (world-status w)
   (place-at-pos PLAYER-ICON
                 (player-pos (world-player w))
                 (render-enemies (world-enemies w)
                                 (render-grid (world-grid w)))))))

;;------ FINAL GAME

;; Initial world

(define INITIAL-GRID
  (list
   (list WALL WALL WALL WALL WALL WALL WALL)
   (list WALL FLOOR KEY FLOOR FLOOR EXIT WALL)
   (list WALL FLOOR WALL WALL FLOOR WALL WALL)
   (list WALL FLOOR FLOOR DOOR FLOOR FLOOR WALL)
   (list WALL WALL WALL WALL WALL WALL WALL)))

(define INITIAL-PLAYER
  (make-player (make-pos 1 1) 0))

(define INITIAL-ENEMIES
  (list (make-enemy (make-pos 1 3))))

(define INITIAL-WORLD
  (make-world INITIAL-GRID
              INITIAL-PLAYER
              INITIAL-ENEMIES
              PLAYING))


;; playing? : World -> Boolean
;; purpose: check whether the game should still accept updates.
;; FP note: pure status query.
(define (playing? w)
  (string=? (world-status w) PLAYING))


;; stopped? : World -> Boolean
;; purpose: stop big-bang after the player has won or lost.
;; FP note: pure stop condition.
(define (stopped? w)
  (not (playing? w)))


;; update-player : World KeyEvent -> World
;; purpose: process one key event through the functional core.
;; FP note: big-bang calls this, but all game rules remain pure.
(define (update-player w ke)
  (if (playing? w)
      (lose-if-collision
       (handle-tile
        (move-player w ke)))
      w))


;; update-world-enemies : World -> World
;; purpose: process one clock tick through the functional core.
;; FP note: this only delegates to the pure enemy update.
(define (update-world-enemies w)
  (if (playing? w)
      (update-enemies w)
      w))


;; start-game : World -> World
;; purpose: imperative shell around the pure functional core.
(define (start-game initial-world)
  (big-bang initial-world
    [on-key update-player]
    [on-tick update-world-enemies]
    [to-draw render]
    [stop-when stopped? render]))


(start-game INITIAL-WORLD)
