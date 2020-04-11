;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)
(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)
(define HIT-RANGE 10)
(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer
(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body
(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define MISSILE (ellipse 5 15 "solid" "red"))


;; ========================================

;; Data Definitions:

(define-struct game (invaders missiles tank time))
;; Game is (make-game  (listof Invader) (listof Missile) Tank Time)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position
;;         and the time passed as measured in ticks (default 1/28 secs per tick)

;; Game constants defined below last data definition

#;
(define (fn-for-game g)
  (... (fn-for-loi (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-tank g))
       (fn-for-time (game-time g))))

;; ----------------------------------------

(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50  1))           ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank tk)
  (... (tank-x tk) (tank-dir tk)))

;; ----------------------------------------

(define-struct invader (x y dir))
;; Invader is (make-invader Number Number Integer[-1, 1])
;; interp. the invader is at (x, y) in screen coordinates
;;         direction dir is left if dir -1, right if dir 1 (similar to Tank)

(define I1 (make-invader 150 100 1))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -1))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 1)) ;> landed, moving right

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dir invader)))

;; ----------------------------------------

;; ListOfInvader is one of:
;;  - empty
;;  - (cons Invader ListOfInvader)
;; interp. a list of all the invaders

(define LOI-1 empty)
(define LOI-2 (cons I1 empty))
(define LOI-3 (cons I1 (cons I2 empty)))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

;; ----------------------------------------

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is at (x, y) in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ----------------------------------------

;; ListOfMissile is one of:
;;  - empty
;;  - (cons Missile ListOfMissile)
;; interp. a list of all the missiles

(define LOM-1 empty)
(define LOM-2 (cons M1 empty))
(define LOM-3 (cons M1 (cons M2 empty)))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; ----------------------------------------

;; Time is Natural
;; interp. number of clock ticks since start of game

(define START-TIME 0)
(define END-TIME 560) ; 20 seconds in given the default tick speed of 28t/s 

#;
(define (fn-for-time t)
  (... t))

;; ----------------------------------------

; Game constants from first data definition

(define G0 (make-game empty empty T0 0))
(define G1 (make-game empty empty T1 0))
(define G2 (make-game (list I1) (list M1) T1 0))
(define G3 (make-game (list I1 I2) (list M1 M2) T1 0))

;; ========================================

;; Functions:

;; Game -> Game
;; start the world by calling make-game, for example: (main G0)

(define (main g)
  (big-bang g                      ; Game
            (on-tick   tock)       ; Game -> Game
            (to-draw   render-loi) ; Game -> Image
            (on-key    handle-key) ; Game KeyEvent -> Game
            (stop-when end)))      ; Game -> Boolean

;; ----------------------------------------
    
;; Game -> Game
;; Move Game to next state

(check-expect (tock (make-game empty empty (make-tank 50 1) 12))
              (make-game empty empty (make-tank (+ 50 TANK-SPEED) 1) 13))
(check-expect (tock (make-game empty (cons (make-missile 100 100) empty) (make-tank 50 1) 155))
              (make-game empty
                         (cons (make-missile 100 (- 100 MISSILE-SPEED)) empty)
                         (make-tank (+ 50 TANK-SPEED) 1) 156))
(check-expect (tock (make-game (cons (make-invader 100 130 1) empty)
                               (cons (make-missile 100 100) empty)
                               (make-tank 50 1) 220))
              (make-game (cons (make-invader (+ 100 INVADER-X-SPEED) (+ 130 INVADER-Y-SPEED) 1) empty)
                         (cons (make-missile 100 (- 100 MISSILE-SPEED)) empty)
                         (make-tank (+ 50 TANK-SPEED) 1) 221))
(check-random (tock (make-game (cons (make-invader 150 90 1) ;3 invaders
                                  (cons (make-invader 20 135 1) ;hit invader
                                        (cons (make-invader 170 180 -1)
                                              empty)))
                               (cons (make-missile 100 420) ;4 missiles
                                     (cons (make-missile 125 311)
                                           (cons (make-missile 29 127) ;missile strike
                                                 (cons (make-missile 155 35)
                                                       empty))))
                               (make-tank WIDTH 1) 400)) ;turning tank, invader creation
              (make-game (cons (make-invader (random WIDTH) 0 1)
                               (cons (make-invader (+ 150 INVADER-X-SPEED) (+ 90 INVADER-Y-SPEED) 1)
                                     (cons (make-invader (- 170 INVADER-X-SPEED) (+ 180 INVADER-Y-SPEED) -1)
                                           empty)))
                         (cons (make-missile 100 (- 420 MISSILE-SPEED))
                               (cons (make-missile 125 (- 311 MISSILE-SPEED))
                                     (cons (make-missile 155 (- 35 MISSILE-SPEED))
                                           empty)))
                         (make-tank WIDTH -1) 401))

;(define (tock g) (make-game loi lom tk t)) ;stub

(define (tock g)
  (make-game (next-invader (game-invaders g) (game-missiles g) (game-time g))
             (next-missile (game-missiles g) (game-invaders g))
             (move-tank (game-tank g))
             (count (game-time g))))

;; ----------------------------------------

;; ListOfInvader ListOfMissile Time -> ListOfInvader
;; produce filtered and ticked list of invaders

(check-expect (next-invader empty empty 12) empty)
(check-expect (next-invader (cons (make-invader 100 100 1) empty) empty 15) ;single invader, no missiles
              (cons (make-invader (+ 100 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 1) empty))
(check-random (next-invader (cons (make-invader 150 90 1) ;2 invaders, create third, no missiles
                                  (cons (make-invader 70 135 -1)
                                        empty))
                            empty
                            200)
              (cons (make-invader (random WIDTH) 0 1)
                    (cons (make-invader (+ 150 INVADER-X-SPEED) (+ 90 INVADER-Y-SPEED) 1)
                          (cons (make-invader (- 70 INVADER-X-SPEED) (+ 135 INVADER-Y-SPEED) -1)
                                empty))))
(check-expect (next-invader (cons (make-invader 150 90 1) ;2 invaders, one turns, no missiles
                                  (cons (make-invader WIDTH 210 1)
                                        empty))
                            empty
                            133)
              (cons (make-invader (+ 150 INVADER-X-SPEED) (+ 90 INVADER-Y-SPEED) 1)
                    (cons (make-invader (- WIDTH INVADER-X-SPEED) (+ 210 INVADER-Y-SPEED) -1)
                          empty)))
(check-expect (next-invader (cons (make-invader 150 90 1) ;3 invaders, 4 missiles, one hit
                                  (cons (make-invader 20 135 1)
                                        (cons (make-invader 170 180 -1)
                                              empty)))
                            (cons (make-missile 100 420)
                                  (cons (make-missile 125 311)
                                        (cons (make-missile 29 127)
                                              (cons (make-missile 155 35)
                                                    empty))))
                            422)
              (cons (make-invader (+ 150 INVADER-X-SPEED) (+ 90 INVADER-Y-SPEED) 1)
                    (cons (make-invader (- 170 INVADER-X-SPEED) (+ 180 INVADER-Y-SPEED) -1)
                          empty)))

;(define (next-invader loi lom t) loi) ;stub

(define (next-invader loi lom t)
  (add-invader? (advance-invaders (filter-invaders-lom loi lom)) t))

;; ----------------------------------------

;; ListOfInvader Missile -> ListOfInvader
;; removes an invader from loi that have been struck by a single missile
;; note that loi is always sorted lowest to highest based on invader-y
;; note strong similarity and same technique as filter-missiles-i

(check-expect (filter-invaders-m empty (make-missile 100 100)) empty)
(check-expect (filter-invaders-m (cons (make-invader 100 135 1) (cons (make-invader 150 235 1) empty))
                               (make-missile 130 200))
              (cons (make-invader 100 135 1) (cons (make-invader 150 235 1) empty)))
(check-expect (filter-invaders-m (cons (make-invader 100 135 1) (cons (make-invader 150 235 1) empty))
                               (make-missile 100 135))
              (cons (make-invader 150 235 1) empty))
(check-expect (filter-invaders-m (cons (make-invader 100 135 1) (cons (make-invader 150 235 1) empty))
                               (make-missile 150 235))
              (cons (make-invader 100 135 1) empty))
(check-expect (filter-invaders-m (cons (make-invader 100 135 1)
                                     (cons (make-invader 150 235 1)
                                           (cons (make-invader 170 255 -1)
                                                 (cons (make-invader 20 312 1) empty))))
                               (make-missile 150 235))
              (cons (make-invader 100 135 1)
                               (cons (make-invader 170 255 -1)
                                     (cons (make-invader 20 312 1) empty))))

;(define (filter-invaders-m loi m) loi) ;stub

(define (filter-invaders-m loi m)
  (cond [(empty? loi) empty]
        [else
         (if (missile-kill? (first loi) m)
             (rest loi)
             (cons (first loi) (filter-invaders-m (rest loi) m)))]))

;; ----------------------------------------

;; ListOfInvader ListOfMissile -> ListOfInvader
;; removes any invaders that have been struck by any of the missiles in lom
;; note that loi is always sorted lowest to highest based on invader-y
;; note that lom is always sorted highest to lowest based on missile-y
;; note strong similarity and same technique as filter-missiles-loi

(check-expect (filter-invaders-lom empty empty) empty)
(check-expect (filter-invaders-lom (cons (make-invader 150 235 1) empty) empty)
              (cons (make-invader 150 235 1) empty))
(check-expect (filter-invaders-lom empty (cons (make-missile 100 100) empty))
              empty)
(check-expect (filter-invaders-lom (cons (make-invader 150 235 1) empty)
                                   (cons (make-missile 155 240) empty))
              empty)
(check-expect (filter-invaders-lom (cons (make-invader 100 135 1)
                                         (cons (make-invader 150 235 1)
                                               (cons (make-invader 170 255 -1)
                                                     (cons (make-invader 20 312 1) empty))))
                                   (cons (make-missile 50 360)
                                         (cons (make-missile 40 310)
                                               (cons (make-missile 150 235)
                                                     (cons (make-missile 130 100) empty)))))
              (cons (make-invader 100 135 1)
                    (cons (make-invader 170 255 -1)
                          (cons (make-invader 20 312 1) empty))))

;(define (filter-invaders-lom loi lom) loi) ;stub

(define (filter-invaders-lom loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else
         (if (equal? (filter-invaders-m loi (first lom)) loi)
             (filter-invaders-lom loi (rest lom))
             (filter-invaders-m loi (first lom)))]))

;; ----------------------------------------

;; Invader Missile -> Boolean
;; true if missile makes contact with invader (ie. within HIT-RANGE), false otherwise

(check-expect (missile-kill? (make-invader 100 120 1) (make-missile 150 200)) false) ;miss
(check-expect (missile-kill? (make-invader 100 120 1) (make-missile 100 120)) true)  ;direct hit
(check-expect (missile-kill? (make-invader 100 120 1)
                             (make-missile (+ 100 HIT-RANGE) (- 120 HIT-RANGE)))
              true)                                                         ;kill, just within range

;(define (missile-kill? i m) false) ;stub

(define (missile-kill? i m)
  (and (<= (abs (- (invader-y i) (missile-y m))) HIT-RANGE)
       (<= (abs (- (invader-x i) (missile-x m))) HIT-RANGE)))

;; ----------------------------------------

;; ListOfInvader Time -> ListOfInvader
;; possibly adds a new invader at random position at top of BACKGROUND

(check-expect (add-invader? empty  12) empty)
(check-random (add-invader? empty   0)
              (cons (make-invader (random WIDTH) 0 1) empty)) ;creates an invader as
(check-random (add-invader? empty 100)
              (cons (make-invader (random WIDTH) 0 1) empty))
(check-random (add-invader?
               (cons (make-invader 100 135 1)
                     (cons (make-invader 150 235 1) empty)) 300)
              (cons (make-invader (random WIDTH) 0 1)
                    (cons (make-invader 100 135 1)
                          (cons (make-invader 150 235 1) empty))))
                                  
;(define (add-invader? loi t) loi) ;stub

(define (add-invader? loi t)
  (if (= (modulo t INVADE-RATE) 0)
      (create-invader loi)
      loi))
  
;; ----------------------------------------

;; Time -> Time
;; counts the number of clock ticks since start

(check-expect (count  0)  1)
(check-expect (count 12) 13)
(check-expect (count 17) 18)

;(define (count t) t) ;stub

(define (count t)
  (add1 t))

;; ----------------------------------------

;; ListOfInvader -> ListOfInvader
;; adds a new invader to current loi
;; positioned at random place along the top of BACKGROUND

(check-random (create-invader empty)
              (cons (make-invader (random WIDTH) 0 1) empty))
(check-random (create-invader (cons (make-invader 100 135 1)
                                    (cons (make-invader 150 235 1) empty)))
              (cons (make-invader (random WIDTH) 0 1)
                    (cons (make-invader 100 135 1)
                          (cons (make-invader 150 235 1) empty))))
                                          
;(define (create-invader loi) loi) ;stub

(define (create-invader loi)
  (cons (make-invader (random WIDTH) 0 1) loi))

;; ----------------------------------------

;; ListOfInvader -> ListOfInvader
;; advance invaders across and down the screen by INVADER-X-SPEED and INVADER-Y-SPEED respectively
;; on reaching the left or right edge, the invader changes direction

(check-expect (advance-invaders empty) empty)
(check-expect (advance-invaders
               (cons (make-invader 100 200 1) empty))
              (cons (make-invader (+ 100 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) 1) empty))
(check-expect (advance-invaders
               (cons (make-invader 100 200 -1) empty))
              (cons (make-invader (- 100 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) -1) empty))
(check-expect (advance-invaders
               (cons (make-invader WIDTH 150 1)
                     (cons (make-invader 100 200 1) empty)))
              (cons (make-invader (- WIDTH INVADER-X-SPEED) (+ 150 INVADER-Y-SPEED) -1) ;invader changes dir
                    (cons (make-invader (+ 100 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) 1) empty)))
(check-expect (advance-invaders
               (cons (make-invader 0 100 -1) empty))
              (cons (make-invader (+ 0 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 1) empty)) ;invader changes dir

;(define (advance-invaders loi) loi) ;stub

(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [(and (<= (invader-x   (first loi))  0)
              ( = (invader-dir (first loi)) -1))  ;at left edge and going left
         (cons (make-invader (+ (invader-x (first loi)) INVADER-X-SPEED)
                             (+ (invader-y (first loi)) INVADER-Y-SPEED)
                             1)
               (advance-invaders (rest loi)))]
        [(and (>= (invader-x   (first loi)) WIDTH)
              ( = (invader-dir (first loi)) 1))   ;at right edge and going right
         (cons (make-invader (- (invader-x (first loi)) INVADER-X-SPEED)
                             (+ (invader-y (first loi)) INVADER-Y-SPEED)
                             -1)
               (advance-invaders (rest loi)))]
        [else
         (cons (make-invader (+ (invader-x (first loi)) (* INVADER-X-SPEED (invader-dir (first loi))))
                             (+ (invader-y (first loi)) INVADER-Y-SPEED)
                             (invader-dir (first loi)))
               (advance-invaders (rest loi)))]))

;; ----------------------------------------

;; ListOfMissile ListOfInvader -> ListOfMissile
;; produces filtered and ticked list of missiles
;; missiles are filtered twice:
;;  - once for contact with an invader
;;  - once for when missiles have left BACKGROUND

(check-expect (next-missile empty empty) empty)
(check-expect (next-missile (cons (make-missile 20 312) ;need to amend this template of a check-expect
                                  (cons (make-missile 170 255) ;5 missiles
                                        (cons (make-missile 150 235) ;one hit
                                              (cons (make-missile 100 135)
                                                    (cons (make-missile 133 -15) empty))))) ;one missed
                            (cons (make-invader 130 100 1)
                                  (cons (make-invader 150 235 -1)
                                        (cons (make-invader 40 310 1)
                                              (cons (make-invader 50 360 1) empty)))))
              (cons (make-missile 20 (- 312 MISSILE-SPEED)) ;3 missiles remain to be advanced
                    (cons (make-missile 170 (- 255 MISSILE-SPEED))
                          (cons (make-missile 100 (- 135 MISSILE-SPEED)) empty))))

(define (next-missile lom loi)
  (advance-missiles (filter-missed-missiles (filter-missiles-loi lom loi))))

;; ----------------------------------------

;; ListOfMissile ListOfInvader -> ListOfMissile
;; removes any missiles that have made contact with an invader in loi
;; note that lom is always sorted highest to lowest based on missile-y
;; note that loi is always sorted lowest to highest based on invader-y
;; note strong similarity and same technique as filter-invaders-lom

(check-expect (filter-missiles-loi empty empty) empty)
(check-expect (filter-missiles-loi (cons (make-missile 150 235) empty) empty)
              (cons (make-missile 150 235) empty))
(check-expect (filter-missiles-loi empty (cons (make-invader 100 100 1) empty))
              empty)
(check-expect (filter-missiles-loi (cons (make-missile 150 235) empty)
                                   (cons (make-invader 155 240 1) empty))
              empty)
(check-expect (filter-missiles-loi (cons (make-missile 20 312)
                                         (cons (make-missile 170 255)
                                               (cons (make-missile 150 235)
                                                     (cons (make-missile 100 135) empty))))
                                   (cons (make-invader 130 100 1)
                                         (cons (make-invader 150 235 -1)
                                               (cons (make-invader 40 310 1)
                                                     (cons (make-invader 50 360 1) empty)))))
              (cons (make-missile 20 312)
                    (cons (make-missile 170 255)
                          (cons (make-missile 100 135) empty))))

;(define (filter-missiles-loi lom loi) lom) ;stub

(define (filter-missiles-loi lom loi)
  (cond [(empty? lom) empty]
        [(empty? loi) lom]
        [else
         (if (equal? (filter-missiles-i lom (first loi)) lom)
             (filter-missiles-loi lom (rest loi))
             (filter-missiles-i lom (first loi)))]))

;; ----------------------------------------

;; ListOfMissile Invader -> ListOfMissile
;; removes a missile from lom that has struck an invader
;; note that lom is always sorted highest to lowest based on missile-y
;; note strong similarity and same technique as filter-invaders-m

(check-expect (filter-missiles-i empty (make-invader 100 100 1)) empty)
(check-expect (filter-missiles-i (cons (make-missile 150 235) (cons (make-missile 100 135) empty))
                               (make-invader 130 200 1))
              (cons (make-missile 150 235) (cons (make-missile 100 135) empty)))
(check-expect (filter-missiles-i (cons (make-missile 150 235) (cons (make-missile 100 135) empty))
                               (make-invader 100 135 1))
              (cons (make-missile 150 235) empty))
(check-expect (filter-missiles-i (cons (make-missile 150 235) (cons (make-missile 100 135) empty))
                               (make-invader 150 235 1))
              (cons (make-missile 100 135) empty))
(check-expect (filter-missiles-i (cons (make-missile 20 312)
                                     (cons (make-missile 170 255)
                                           (cons (make-missile 150 235)
                                                 (cons (make-missile 100 135) empty))))
                               (make-invader 150 235 1))
              (cons (make-missile 20 312)
                               (cons (make-missile 170 255)
                                     (cons (make-missile 100 135) empty))))

;(define (filter-missiles-i lom i) lom) ;stub

(define (filter-missiles-i lom i)
  (cond [(empty? lom) empty]
        [else
         (if (missile-kill? i (first lom))
             (rest lom)
             (cons (first lom) (filter-missiles-i (rest lom) i)))]))

;; ----------------------------------------

;; ListOfMissile -> ListOfMissile
;; missiles that have made contact with invaders have already been filtered
;; this removes any missiles that have moved beyond BACKGROUND (and can no longer be seen)

(check-expect (filter-missed-missiles empty) empty)
(check-expect (filter-missed-missiles (cons (make-missile 100 311)
                                            (cons (make-missile 220 120)
                                                  empty)))
              (cons (make-missile 100 311)
                    (cons (make-missile 220 120)
                          empty)))
(check-expect (filter-missed-missiles (cons (make-missile 100 311)
                                            (cons (make-missile 220 120)
                                                  (cons (make-missile 155 -20)
                                                        empty))))
              (cons (make-missile 100 311)
                    (cons (make-missile 220 120)
                          empty)))

;(define (filter-missed-missiles lom) lom) ;stub

(define (filter-missed-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (< (missile-y (first lom)) -10) ;remove missile once over 10px above BACKGROUND
             (rest lom)
             (cons (first lom) (filter-missed-missiles (rest lom))))]))

;; ----------------------------------------

;; ListOfMissle -> ListOfMissile
;; missiles are advanced upwards by MISSILE-SPEED

(check-expect (advance-missiles empty) empty)
(check-expect (advance-missiles (cons (make-missile 100 130) empty))
                                (cons (make-missile 100 (- 130 MISSILE-SPEED)) empty))
(check-expect (advance-missiles
               (cons (make-missile 220 410)
                     (cons (make-missile 90 330)
                           (cons (make-missile 100 130)
                                 empty))))
              (cons (make-missile 220 (- 410 MISSILE-SPEED))
                    (cons (make-missile 90 (- 330 MISSILE-SPEED))
                          (cons (make-missile 100 (- 130 MISSILE-SPEED))
                                empty))))

;(define (advance-missiles lom) lom) ;stub

(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (make-missile (missile-x (first lom)) (- (missile-y (first lom)) MISSILE-SPEED))
               (advance-missiles (rest lom)))]))

;; ----------------------------------------

;; Tank -> Tank
;; moves tank along x direction by TANK-SPEED
;; moves right if dir is 1, left if dir is -1

(check-expect (move-tank (make-tank 150 1))
              (make-tank (+ 150 TANK-SPEED) 1))
(check-expect (move-tank (make-tank 150 -1))
              (make-tank (- 150 TANK-SPEED) -1))
(check-expect (move-tank (make-tank WIDTH 1))
              (make-tank WIDTH -1)) ;tank turns rather than moves
(check-expect (move-tank (make-tank 0 -1))
              (make-tank 0 1))      ;tank turns rather than moves

;(define (move-tank tk) tk) ;stub

(define (move-tank tk)
  (cond [(and (<= (tank-x tk) 0) (= (tank-dir tk) -1)) ;left edge and going left
         (turn-tank tk)]
        [(and (>= (tank-x tk) WIDTH) (= (tank-dir tk) 1)) ;right edge and going right
         (turn-tank tk)]
        [else
         (make-tank (+ (tank-x tk) (* TANK-SPEED (tank-dir tk))) (tank-dir tk))]))

;; ----------------------------------------

;; Tank -> Tank
;; changes tank dir once it reaches the edge of BACKGROUND

(check-expect (turn-tank (make-tank 0 -1))
              (make-tank 0 1))
(check-expect (turn-tank (make-tank WIDTH 1))
              (make-tank WIDTH -1))

;(define (turn-tank tk) tk) ;stub

(define (turn-tank tk)
  (make-tank (tank-x tk) (* (tank-dir tk) -1)))

;; ----------------------------------------

;; Game -> Image
;; consumes a game and renders an image of TANK on BACKGROUND

(check-expect (render-tank (make-game empty empty T1 0))
              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-tank (make-game
                       (cons (make-invader 150 90 1)
                             (cons (make-invader 170 180 -1)
                                   empty))
                       (cons (make-missile 100 420)
                             (cons (make-missile 125 311)
                                   (cons (make-missile 155 35)
                                         empty)))
                       (make-tank 90 1)
                       311))
              (place-image TANK 90 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;(define (render-tank g) img) ;stub

(define (render-tank g)
  (place-image
   TANK (tank-x (game-tank g)) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;; ----------------------------------------

;; Game -> Image
;; consumes a game and renders missiles on render-tank

(check-expect (render-lom (make-game empty empty T1 0))
              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-lom (make-game
                       (cons (make-invader 150 90 1)
                             (cons (make-invader 170 180 -1)
                                   empty))
                       (cons (make-missile 100 420)
                             (cons (make-missile 125 311)
                                   (cons (make-missile 155 35)
                                         empty)))
                       (make-tank 90 1)
                       311))
              (place-image
               MISSILE 100 420
               (place-image
                MISSILE 125 311
                (place-image
                 MISSILE 155 35
                 (place-image
                   TANK 90 (- HEIGHT TANK-HEIGHT/2) BACKGROUND)))))

;(define (render-lom g) img) ;stub

(define (render-lom g)
  (cond [(empty? (game-missiles g)) (place-image empty-image 0 0 (render-tank g))]
        [else
         (place-image
          MISSILE
          (missile-x (first (game-missiles g)))
          (missile-y (first (game-missiles g)))
          (render-lom (make-game (game-invaders g)
                                 (rest (game-missiles g))
                                 (game-tank g)
                                 (game-time g))))]))
  
;; ----------------------------------------

;; Game -> Image
;; consumes a game and renders invaders on render-missiles
;; note similarity to render-lom

(check-expect (render-loi (make-game empty empty T1 0))
              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-loi (make-game
                       (cons (make-invader 150 90 1)
                             (cons (make-invader 170 180 -1)
                                   (cons (make-invader 34 220 1)
                                   empty)))
                       (cons (make-missile 100 420)
                             (cons (make-missile 125 311)
                                   (cons (make-missile 155 35)
                                         empty)))
                       (make-tank 90 1)
                       311))
              (place-image
               INVADER 150 90
               (place-image
                INVADER 170 180
                (place-image
                 INVADER 34 220
                 (place-image
                  MISSILE 100 420
                  (place-image
                   MISSILE 125 311
                   (place-image
                    MISSILE 155 35
                    (place-image
                     TANK 90 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))))))

;(define (render-loi g) img) ;stub

(define (render-loi g)
  (cond [(empty? (game-invaders g)) (place-image empty-image 0 0 (render-lom g))]
        [else
         (place-image
          INVADER
          (invader-x (first (game-invaders g)))
          (invader-y (first (game-invaders g)))
          (render-loi (make-game (rest (game-invaders g))
                                 (game-missiles g)
                                 (game-tank g)
                                 (game-time g))))]))

;; ----------------------------------------

;; Game KeyEvent -> Game
;; There are three KeyEvents - tank movement (left and right) and missile release.
;; These can be captured in seperate parts of the cond statement

(check-expect (handle-key G1 "x") G1) ;undefined key has no effect
(check-expect (handle-key G1 " ") ;first missile created
              (make-game empty
                         (cons (make-missile 50 (- HEIGHT (image-height TANK))) empty)
                         T1
                         0))
(check-expect (handle-key (make-game empty ;existing missiles, missile created
                                     (cons (make-missile 125 311)
                                           (cons (make-missile 155 35)
                                                 empty))
                                     T1
                                     0)
                          " ")
              (make-game empty
                         (cons (make-missile 50 (- HEIGHT (image-height TANK)))
                               (cons (make-missile 125 311)
                                     (cons (make-missile 155 35)
                                           empty)))
                         T1
                         0))
(check-expect (handle-key (make-game (cons (make-invader 150 90 1) empty)
                                     (cons (make-missile 125 311) empty)
                                     (make-tank 120 1) ;tank moving right
                                     230)
                          "left")
              (make-game (cons (make-invader 150 90 1) empty)
                         (cons (make-missile 125 311) empty)
                         (make-tank 120 -1) ;"left" changes tank dir
                         230))
(check-expect (handle-key (make-game (cons (make-invader 150 90 1) empty)
                                     (cons (make-missile 125 311) empty)
                                     (make-tank 120 -1) ;tank moving left
                                     230)
                          "left")
              (make-game (cons (make-invader 150 90 1) empty)
                         (cons (make-missile 125 311) empty)
                         (make-tank 120 -1) ;"left" has no effect
                         230))
(check-expect (handle-key (make-game (cons (make-invader 150 90 1) empty)
                                     (cons (make-missile 125 311) empty)
                                     (make-tank 120 -1) ;tank moving left
                                     230)
                          "right")
              (make-game (cons (make-invader 150 90 1) empty)
                         (cons (make-missile 125 311) empty)
                         (make-tank 120 1) ;"right" changes dir
                         230))
(check-expect (handle-key (make-game (cons (make-invader 150 90 1) empty)
                                     (cons (make-missile 125 311) empty)
                                     (make-tank 120 1) ;tank moving right
                                     230)
                          "right")
              (make-game (cons (make-invader 150 90 1) empty)
                         (cons (make-missile 125 311) empty)
                         (make-tank 120 1) ;"right" has no effect
                         230))

(define (handle-key g ke)
  (cond [(key=? ke " ")
         (make-game
          (game-invaders g)
          (cons (make-missile (tank-x (game-tank g)) (- HEIGHT (image-height TANK))) (game-missiles g))
          (game-tank g)
          (game-time g))]
        [(key=? ke "left")
         (make-game
          (game-invaders g)
          (game-missiles g)
          (make-tank (tank-x (game-tank g)) -1)
          (game-time g))]
        [(key=? ke "right")
         (make-game
          (game-invaders g)
          (game-missiles g)
          (make-tank (tank-x (game-tank g)) 1)
          (game-time g))]
        [else 
         g]))

;; ----------------------------------------

;; Game -> Boolean
;; Game is stopped when an invader reaches the bottom

(check-expect (end G0) false)
(check-expect (end (make-game
                    (cons (make-invader 150 90 1)
                          (cons (make-invader 170 180 -1)
                                (cons (make-invader 34 220 1)
                                      empty)))
                    (cons (make-missile 100 420)
                          (cons (make-missile 125 311)
                                (cons (make-missile 155 35)
                                      empty)))
                    (make-tank 90 1)
                    311))
              false)
(check-expect (end (make-game
                    (cons (make-invader 150 90 1)
                          (cons (make-invader 170 180 -1)
                                (cons (make-invader 34 (+ HEIGHT 5) 1)
                                      empty)))
                    (cons (make-missile 100 420)
                          (cons (make-missile 125 311)
                                (cons (make-missile 155 35)
                                      empty)))
                    (make-tank 90 1)
                    311))
              true)

;(define (end g) false) ;stub

(define (end g)
  (cond [(empty? (game-invaders g)) false]
        [else
         (if (> (invader-y (first (game-invaders g))) HEIGHT)
             true
             (end (make-game (rest (game-invaders g))
                             (game-missiles g)
                             (game-tank g)
                             (game-time g))))]))
