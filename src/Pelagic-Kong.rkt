#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;************************
;TO DO LIST:
; -Move down at spouts?
; -Make Walley infront or behind waves depending on posiiton
; ->Jake: this *may* be possible, I would have to separate the spout from the tile and draw the waves and spouts at different times.
;   for now I have just put walley in front of everything-kelly
; -Win/Lose animations
; ->I'm going to try to add these after you reach the rainbow and before advancing to the next stage-kelly
; -Support WASD for up down side to side
; -> since I got the win when you reach the rainbow done, we can remove the w and s test for the win/loss. It was just for ease of testing.
; -Score calc based on time and lives
; -High Score added to Splash-screen
; -Sounds
;************************


;************
; CONSTANTS
;************
;;defines constants used in the making of the worlds
;;need to be changed to functions to build the actually SPLASH_SCREEN, Game-stage, etc


(define WINDOW (make-posn 500 500))
(define TILE_HEIGHT 75)
(define TILE_WIDTH 50)
(define START (make-posn (- (posn-x WINDOW) 50) (- (posn-y WINDOW) (* TILE_HEIGHT 1.5))))
(define STEP_SIZE_Y (/ TILE_HEIGHT 10)) 
(define STEP_SIZE_X (/ TILE_WIDTH 10))
(define BACKGROUND (square (posn-x WINDOW) "solid" "Medium Aquamarine"))
(define NORMAL (text "NORMAL" 14 "RED"))
(define LASER-SHARKS (text "SHARKS-WITH-LASERBEAMS" 14 "RED"))
(define HIGHLIGHT (rectangle 200 15 "solid" "yellow"))
(define SHARKNADO (text "SHARK-NADO" 14 "RED"))


;*************
;Images Defined
;*************
;;screen shot for use on helpscreen?
(define (screen_shot s)
  (freeze (BEHOLD-Stage 1 player (world-score s))))

;;custom colors
;;(make-color r g b a)
(define SEAFOAM (make-color 104 220 171 85))
(define CLEAR (make-color 100 100 100 0))
(define light-wind (make-color 0 158 158 200))
(define dark-wind (make-color 0 133 133 250))
;;(define SEAFOAM "CadetBlue")

;;Key Image
;;used on help-screens
;; takes direction as arg
;;returns image object
(define (key d)
  (let ([key (list(make-posn 0 0)
                (make-posn 2 -2)
                (make-posn 10 -2)
                (make-posn 12 0)
                (make-posn 12 10)
                (make-posn 10 12)
                (make-posn 2 12)
                (make-posn 0 10))])
  (overlay/align "middle" "top"
                 (overlay (rotate (cond ((eq? d 'down) 180)
                                        ((eq? d 'right) -90)
                                        ((eq? d 'left) 90)
                                        (else 0))
                                  (isosceles-triangle 7 35 "solid" "Black"))
                          (polygon key "solid" "lightgray") (polygon key "outline" "Dimgray"))
                 (scale 1.4 (polygon key "solid" "gray")))))

;;Spout Image
;;used on spout tiles
;; takes no args
;;returns image object
(define spout
  (let* ([drop (list (make-pulled-point 1/2 -30 15 10 1/2 -30)
                   (make-pulled-point 1/4 60 22 40 1/2 -80)
                   (make-pulled-point 1/2 80 6 28 1/4 -15))]
        [mini-drop (list (make-pulled-point 1/2 -30 15 10 1/2 -30)
                   (make-pulled-point 1/4 90 17 40 1/2 -60)
                   (make-pulled-point 1/4 50 13 28 1/4 0))])
  (overlay/align/offset
   "middle" "middle"
   (above/align "middle"
                ;;most-upper
                (overlay/align/offset
                 "middle" "bottom"
                 (scale 1/3 (flip-horizontal (rotate 120 (overlay/align/offset
                                                          "middle" "middle"
                                                          (rotate -15 (scale 1/2 (polygon drop "solid" "Honeydew")))
                                                          -3 -6
                                                          (scale 1 (polygon drop "solid" "SeaGreen"))))))
                 10 -2
                 (scale 1/2 (rotate 122 (overlay/align/offset
                                         "middle" "middle"
                                         (rotate -10 (scale 1/2 (polygon drop "solid" "Honeydew")))
                                         -3 -4
                                         (scale 1 (polygon drop "solid" "SeaGreen"))))))
                ;;mid-upper
                (overlay/align/offset
                 "middle" "bottom"
                 (scale 3/4 (flip-horizontal (rotate 110 (overlay/align/offset
                                                          "middle" "middle"
                                                          (rotate -15 (scale 1/2 (polygon mini-drop "solid" "Honeydew")))
                                                          -3 -6
                                                          (scale 1 (polygon mini-drop "solid" "LightSeaGreen"))))))
                 23 3
                 (scale 3/4 (rotate 100 (overlay/align/offset
                                         "middle" "middle"
                                         (rotate -10 (scale 1/2 (polygon mini-drop "solid" "Honeydew")))
                                         -3 -4
                                         (scale 1 (polygon mini-drop "solid" "LightSeaGreen"))))))
                ;lower
                (beside
                 ;left lower
                 (overlay/align "right" "top"
                                (scale 1 (flip-horizontal (rotate 35 (overlay/align/offset
                                                                      "middle" "middle"
                                                                      (rotate -15 (scale 1/2 (polygon drop "solid" "Honeydew")))
                                                                      -3 -6
                                                                      (scale 1 (polygon drop "solid" "SeaGreen"))))))
                                (scale 1.2 (flip-horizontal (rotate 80 (overlay/align/offset
                                                                        "middle" "middle"
                                                                        (rotate -10 (scale/xy 1/3 2/3 (polygon drop "solid" "Honeydew")))
                                                                        -4 -5
                                                                        (scale 1 (polygon drop "solid" "LightSeaGreen")))))))
                 ;right lower
                 (overlay/align "left" "top"
                                (scale/xy 3/4 1 (rotate 50 (overlay/align/offset
                                                            "middle" "middle"
                                                            (rotate -30 (scale 1/2 (polygon drop "solid" "Honeydew")))
                                                            -4 -7
                                                            (scale 1 (polygon drop "solid" "SeaGreen")))))
                                (scale 1 (rotate 70 (overlay/align/offset "middle" "middle"
                                                                          (rotate -15 (scale 3/5 (polygon drop "solid" "Honeydew")))
                                                                          -3 -6
                                                                          (scale 1 (polygon drop "solid" "LightSeaGreen"))))))))
   3 32
   ;stem
   (polygon (list (make-pulled-point 1/4 60 15 10 1/4 -60)
                  (make-pulled-point 1/4 0 18 60 1/2 0)
                  (make-pulled-point 1/2 0 12 60 1/2 0))
            "solid" "LightSeaGreen"))))

;;Floor Tile Image
;;used on background of stages
;;takes no args
;;returns image object
(define FLOOR_TILE
  (let ([waves (list (make-posn 0 17)
                                (make-pulled-point 1/2 -30 9 10 1/2 30)
                                (make-pulled-point 1/2 -30 27 11 1/2 30)
                                (make-pulled-point 1/2 -30 46 8 1/2 30)
                                (make-posn 50 17)
                                (make-posn 50 25)
                                (make-posn 0 25))])
  (place-image
   (overlay/align/offset "middle" "top"
    (overlay/align/offset "middle" "bottom"
                  (polygon waves "solid" "Medium Aquamarine")
                  0 -1
                  (flip-horizontal (polygon waves "solid" "White")))
    0 -1
    (polygon waves "solid" "SeaGreen"))
    25 67
   (rectangle 50 75 "outline" CLEAR))))

(define SPOUT_TILE
  (underlay/align "middle" "bottom"
                 spout
                 FLOOR_TILE))


;;Sharkfin Image
;;used in stages as enemy
;;takes no args
;;returns image object
(define sharkfin
  (overlay/align "center" "bottom"
                 (rectangle 25 72 "outline" CLEAR)
                 (polygon
                  (list (make-pulled-point 1/2 -20 0.5 0.5 1/3 -55) ;;top point
                 (make-pulled-point 1/2 30 25 32 1/3 0)  ;;right bottom corner
                 (make-pulled-point 1/2 0 0 32 1/2 20)) ;;left bottom corner
                  "solid"
                 "DarkGray")
                 (polygon
                  (list (make-pulled-point 1/2 -20 0 0 1/4 -35) ;;top point
                 (make-pulled-point 1/2 30 25 32 1/3 0)  ;;right bottom corner
                 (make-pulled-point 1/2 0 0 32 1/2 20)) ;;left bottom corner
                  "solid"
                  "DimGray")
                 (polygon
                  (list (make-pulled-point 1/2 -20 0.5 0.5 1/3 -55) ;;top point
                 (make-pulled-point 1/2 30 25 32 1/3 0)  ;;right bottom corner
                 (make-pulled-point 1/2 0 0 32 1/2 20)) ;;left bottom corner
                  "outline"
                 (make-pen "Dark Slate gray" 1 "solid" "butt" "miter"))))
;;Laser-Shark Image
;;used in stages as enemy, builds on sharkfin image
;;takes no args
;;returns image object
(define laser-shark
  (add-line
   (add-line sharkfin
             5 50
             18 50
             (make-pen "black" 4 "solid" "round" "round"))
            20 50
            50 50
            "orange"))

;;***********
;; SHARKNADO
;;***********
;sharknado animation/image
;used in sharknado diff level stages
; takes image and qty as args
; recursive call to draw next shape in image
; returns image object
 (define (draw-sharknado image s)
   (let* ([angle (if (zero? (remainder s 3)) 0 -2)]
          [color (if (zero? (remainder s 5)) dark-wind light-wind)]
          [x-offset (if (zero? (remainder s 3)) (- 0 (random 1 10)) (random 1 10))]
          [y-offset (random 4 6)]
          [width (* s 7)]
          [height (* 10 (ceiling (/ width 100)))])
   (cond ((zero? s) image)
         ((overlay/align/offset "center" "top"
                                (rotate angle (ellipse width height "solid" color))
                                x-offset y-offset
                                (draw-sharknado image (sub1 s)))))))
;;Sharknado function
;used to animate sharknado
; takes time and image as args
; adds sharkfins and draws sharknado image based on time input
; returns image object
(define (create-sharknado x screen)
  (let* ([speed (* x 5)]
         [d 6]
         [path_length (- (posn-y WINDOW) 65)]
         [start-x 50]
         [depth (quotient (- (posn-y WINDOW) 100) d)]
         [shark-images (for/list ([i (in-range 1 d)])
                         (if (zero? (remainder 3 i)) sharkfin laser-shark))]
         [shark-pos (for/list ([i (in-range 1 d)])
                       (make-posn (+ (modulo speed (- path_length (* depth i))) (* i start-x)) (* i depth)))])
    (if (<= x 35)
        (place-images shark-images
                  shark-pos
                  (overlay (draw-sharknado (ellipse 5 7 "solid" light-wind)
                                           (quotient (posn-y WINDOW) 7))
                           screen))
        screen)))
;;Sharknado Screen-shot
;used in helpscreen
;returns image object
(define sharknado_still
  (freeze (create-sharknado 34 (rectangle 200 200 "outline" CLEAR))))

;Ping Image
;used to accent super-horn and rainbow
;returns image object
(define (ping time)
  (let ([s (quotient time 1)])
  (rotate (modulo (+ s 360) 360) (pulled-regular-polygon 10 4 (/ 2 (random 1 5)) 25 "solid" "Ghostwhite"))))

;Super-Horn Image
;used for super-power in stages 5+ to defend walley from sharks
;placed randomly on stage, as well as adorned on walley's head when found
;takes no args
;returns image object
(define (super-horn t) (scale 1.2 (rotate -30
                           (overlay/align/offset "middle" "middle"
                                          (ping (+ t 7))
                                          10 8
                                          (scene+curve
                    (scene+curve
                     (scene+curve
                      (scene+curve
                       (scene+curve
                        (scene+curve
                         (scene+curve
                    (scene+curve
                     (scene+curve
                      (scene+curve
                       (scene+curve
                        (scene+curve (polygon (list (make-posn 0 0) ;;tip point
                                                    (make-pulled-point 1/3 0 22 45 1/2 -45) ;;right point
                                                    (make-pulled-point 1/3 45 0 45 1/2 0)) ;;left tail point))
                                              "solid" "DarkGray")
                                     0 36 -45 1/2
                                     18 37 45 1/3
                                     "silver")
                        0 37 -44 1/2
                        17 38 44 1/3
                        "DimGray")
                       0 28 -45 1/2
                       14 29 45 1/3
                       "silver")
                      0 29 -45 1/2
                      14 30 45 1/3
                      "DimGray")
                     0 21 -45 1/2
                     11 22 45 1/3
                     "silver")
                    0 22 -45 1/2
                    11 23 45 1/3
                    "DimGray")
                         0 15 -45 1/2
                         8 16 45 1/3
                         "silver")
                        0 16 -45 1/2
                        8 17 45 1/3
                        "DimGray")
                       0 10 -45 1/2
                       5 11 45 1/3
                       "silver")
                      0 11 -45 1/2
                      5 12 45 1/3
                      "DimGray")
                     0 5 -45 1/2
                     3 6 45 1/3
                     "silver")
                    0 6 -45 1/2
                    3 7 45 1/3
                    "DimGray")))))
;;Fish Image
;used for super-power to give walley an extra life
; placed on stage 5+
;returns image object
(define fish
  (add-polygon (underlay/offset(underlay/offset (ellipse 60 30 "solid" "seagreen")
                               0 0
                               (polygon (list (make-pulled-point 1/2 20 -10 15 1/2 -20)
                                              (make-posn -5 25)
                                              (make-pulled-point 1/2 -20 5 15 1/2 20)
                                              (make-posn -5 0))
                                        "solid" "Aquamarine"))
                               20 -5
                               (circle 4 "solid" "LightSalmon"))
               (list (make-pulled-point 1/2 20 -10 15 1/2 -20)
                 (make-posn -17 35)
                 (make-pulled-point 1/2 -20 3 15 1/2 20)
                 (make-posn -17 -5))
           "solid"
           "LightSalmon"))

;;Rainbow Image
; used as goal on stage
; takes no args
; returns image object
(define (rainbow t)
  (place-images (list (ping t) (ping (+ t 9)) (ping (+ t 18)))
                (list (make-posn 25 22) (make-posn 45 48) (make-posn 53 31))
                (place-image
                (overlay/offset (beside/align "middle"
                                             (circle 8 "solid" "white")
                                             (circle 8 "solid" "white"))
                               11 -2
                               (beside/align "middle"
                                             (circle 8 "solid" "white")
                                             (circle 8 "solid" "white")))                                      
               57 65
               (scene+curve
                (scene+curve
                 (scene+curve
                  (scene+curve
                   (scene+curve
                    (scene+curve
                     (scene+curve (rectangle 100 75 "solid" CLEAR)
                                  0 20 25 2/3 ;red
                                  70 65 0 0
                                  (make-pen "red" 5 "solid" "butt" "round"))
                     0 20 23 2/3 ;orange
                     65 65 0 0
                     (make-pen "orange" 5 "solid" "butt" "round"))
                    0 20 21 2/3 ;yellow
                    60 65 0 0
                    (make-pen "yellow" 5 "solid" "butt" "round"))
                   0 20 19 2/3 ;green
                   55 65 0 0
                   (make-pen "green" 5 "solid" "butt" "round"))
                  0 21 17 2/3 ;blue
                  50 65 0 0
                  (make-pen "blue" 5 "solid" "butt" "round"))
                 0 21 15 2/3 ;violet
                 45 65 0 0
                 (make-pen "violet" 5 "solid" "butt" "round"))
                0 22 13 2/3 ;crop
                40 65 0 0
                (make-pen "Medium Aquamarine" 5 "solid" "butt" "round")))))


;****************
;;Walley Character
;****************
; images and draw-functions
;draw-walley function used to move/animate walley based on time and mood state
; takes time and mood as args
; returns image object
(define (draw-walley mood t)
   ;;face
  (let* ([happy-face (add-line
                      (add-line
                       (add-curve        
                        (polygon (list (make-pulled-point 1/2 45 0 0 1/2 -45) ;;top point
                                       (make-pulled-point 1/2 45 40 40 1/2 -45) ;;right middle
                                       (make-pulled-point 1/2 45 0 80 0 0)  ;;bottom middle
                                       (make-pulled-point 3/4 10 -44 76 5/6 10) ;;left bottom corner
                                       (make-pulled-point 1/4 0 -40 40 1/2 -45)) ;;left middle
                                 "solid"
                                 "SkyBlue")
                        35 50 -30 1/2 60 50 30 1/2 "darkblue")
                       7 65 0 80 "darkblue")
                      15 75 0 80 "darkblue")]
         [sad-face (add-line
                    (add-line
                     (add-curve        
                      (polygon (list (make-pulled-point 1/2 45 0 0 1/2 -45) ;;top point
                                     (make-pulled-point 1/2 45 40 40 1/2 -45) ;;right middle
                                     (make-pulled-point 1/2 45 0 80 0 0)  ;;bottom middle
                                     (make-pulled-point 3/4 10 -44 76 5/6 10) ;;left bottom corner
                                     (make-pulled-point 1/4 0 -40 40 1/2 -45)) ;;left middle
                               "solid"
                               "SkyBlue")
                      35 50 30 1/2 60 50 -30 1/2 "red")
                     7 65 0 80 "darkblue")
                    15 75 0 80 "darkblue")]
         [face (cond
                 ((eq? mood 'happy) happy-face)
                 ((eq? mood 'swimming) happy-face)
                 ((eq? mood 'super) happy-face)
                 (else sad-face))]
         [eye-r (overlay/offset (circle 5 "solid" "white")
                  -3 1
                  (circle 9 "solid" "RoyalBlue"))]
         [eye-l (overlay/offset (circle 5 "solid" "white")
                  -3 1
                  (circle 9 "solid" "RoyalBlue"))]
         [ping (pulled-regular-polygon 10 4 3/4 25 "solid" "Ghostwhite")]
         [horn (rotate -30(scene+curve
                           (scene+curve
                            (scene+curve
                             (scene+curve
                              (scene+curve
                               (scene+curve
                                (polygon (list (make-posn 0 0) ;;tip point
                                               (make-pulled-point 1/3 0 22 45 1/2 -45) ;;right point
                                               (make-pulled-point 1/3 45 0 45 1/2 0)) ;;left tail point
                                         "solid"
                                         "burlywood")
                                0 36 -45 1/2
                                18 37 45 1/3
                                "DarkGoldenrod")
                               0 28 -45 1/2
                               14 29 45 1/3
                               "DarkGoldenrod")
                              0 21 -45 1/2
                              11 22 45 1/3
                              "DarkGoldenrod")
                             0 15 -45 1/2
                             8 16 45 1/3
                             "DarkGoldenrod")
                            0 10 -45 1/2
                            5 11 45 1/3
                            "DarkGoldenrod")
                           0 5 -45 1/2
                           3 6 45 1/3
                           "DarkGoldenrod"))]
         ;;create closed polygon using list of posn's(x y), points in clockwise order
         [tail(rotate (cond ((odd? t) 30)
                            (else 60))
                      (polygon (list (make-pulled-point 1/2 10 30 10 1/2 -10) ;;middle tail point
                                     (make-pulled-point 1/2 45 60 10 3/4 45) ;;right tail point
                                     (make-pulled-point 1/2 -10 40 70 1/2 -20) ;;body tail point
                                     (make-pulled-point 1/2 -20 0 10 1/2 -45)) ;;left tail point
                               "solid"
                               "CornflowerBlue"))]
         [body (polygon (list (make-pulled-point 1/2 45 0 0 1/2 -45) ;;top point
                              (make-pulled-point 1/2 45 40 40 1/2 -45) ;;right middle
                              (make-pulled-point 1/2 45 -20 100 0 0)  ;;bottom middle
                              (make-pulled-point 3/4 10 -70 85 5/6 5) ;;left bottom corner
                              (make-pulled-point 1/2 -20 -40 40 1/2 -45)) ;;left middle
                        "solid"
                        "CornflowerBlue")]
         [face-posn (make-posn 112 85)]
         [eye-r-posn (make-posn 100 70)]
         [eye-l-posn (make-posn 145 68)]
         [horn-posn (make-posn 120 35)]
         [body-posn (make-posn 98 95)]
         [tail-posn (make-posn 35 110)])
         ;;underlays images in order of list
         (place-images (list eye-r eye-l (if(eq? mood 'super) (super-horn t) horn) face tail body)
                       (list eye-r-posn eye-l-posn horn-posn face-posn tail-posn body-posn)
                       (rectangle 170 150 "outline" CLEAR))))

;;constants for walley
(define happy-walley
 (draw-walley 'swimming 0))
(define sad-walley
 (draw-walley 'dead 0))
(define super-walley
  (draw-walley 'super 0))

;;Swim method
;;draws walley in a specific direction
; takes mood, direction(string) and time args
; return image object
(define (swim mood direction time)
  (if (eq? direction "left")
      (flip-horizontal (draw-walley mood time))
      (draw-walley mood time)))

(define (posn-offset pos offset)
  (let ((posn-updated-x (+ (posn-x pos) (posn-x offset)))
        (posn-updated-y (+ (posn-y pos) (posn-y offset))))
    (if (< posn-updated-x 0)
        (make-posn (+ posn-updated-x (posn-x WINDOW)) posn-updated-y)
        (make-posn (modulo posn-updated-x (posn-x WINDOW)) posn-updated-y))))

(define (move-player s direction)
  (let ((posn-offset-x (cond ((eq? direction "right") STEP_SIZE_X)
                             ((eq? direction "left") (* -1 STEP_SIZE_X)) 
                             (else 0)))
        (posn-offset-y (cond ((eq? direction "down") STEP_SIZE_Y)
                             ((and (eq? direction "up") (touching-spout-tile? s)) (* -1 STEP_SIZE_Y))
                            (else 0))))
       (make-player (player-state (world-player s))
                    (posn-offset (player-position (world-player s)) (make-posn posn-offset-x posn-offset-y))
                    direction
                    (player-lives (world-player s)))))

(define (touching-spout-tile? s)
  (let* ((touchDistance (* .45 TILE_HEIGHT))
        (tiles (build-board (world-difficulty s)))
        (playerPos (player-position (world-player s)))
        (tileOn (get-tile tiles playerPos s)))
    (if (tile-up? tileOn)
        #t
        (let* ((tileAbovePos (make-posn (posn-x playerPos) (- (posn-y playerPos) touchDistance)))
              (tileAbove (get-tile tiles tileAbovePos s)))
          (if (not (or (null? tileAbove) (equal? tileAbove tileOn)))
              (tile-up? tileAbove) ;tileBelow cannot be in touching distance
              (let* ((tileBelowPos (make-posn (posn-x playerPos) (+ (posn-y playerPos) touchDistance)))
                    (tileBelow (get-tile tiles tileBelowPos s)))
                (tile-up? tileBelow)))))))
              
        
;;Funtion to scroll image, only changes x value based on time and speed args
;;used to scroll walley image aross splash-screen
; takes time arg
; returns new position as int type
(define (scroll-right t)
  (modulo (+ t (+ 200 (posn-x WINDOW))) (+ 200 (posn-x WINDOW))))

;;Funtion to scroll image, only changes x value based on time and speed args
;;@stilsonkl-edit
; takes time arg
; returns new position as int type
(define (scroll-left t)
  (- 700 (modulo (+ t (+ 200 (posn-x WINDOW))) (+ 200 (posn-x WINDOW)))))

;**************
; Player Struct
;**************
;;defines and contracts the player object
;;needs to be expanded to include player characteristics
;;this definition type automatically creates:
;  constructor= (make-player state position direction lives)
;  eq test= player?
;  fields= player-state, player-position, player-lives
(define-struct/contract player ([state (or/c 'swimming
                                             'dead
                                             'super)]
                               [position (or/c #f posn?)]
                               [direction (or/c "right"
                                                "left"
                                                "up"
                                                "down")]
                               [lives (or/c 0 1 2 3 4 5 6 7 8 9)])
  #:transparent)

;;function to make player in certain direction?
;posible duplicate of (swim mood direction)?
(define (player-set-pose p position direction)
  (make-player (player-state p) position direction (player-lives p)))

(define (player-climbing-spout s)
  (let* ((tiles (build-board (world-difficulty s)))
         (tilesTopY (tiles-top-y tiles s))
         (colPerRow (/ (posn-x WINDOW) TILE_WIDTH))
         (rowCount (/ (length tiles) colPerRow))
         (playerTopY (- (posn-y START) (* TILE_HEIGHT (- rowCount 1))))
         (posY (posn-y (player-position (world-player s))))
         (rowY (- posY playerTopY))
         (remain (remainder (round rowY) TILE_HEIGHT))
         (epsilon (/ TILE_HEIGHT 20)))
    (and (> remain epsilon) (< remain (- TILE_HEIGHT epsilon))))) ;User cannot move horizontal unless near top or bottom of spout

;;detects if the "bounding box" of walley crosses into the
; "bounding box" of another object
; @stilsonkl--change from object-pos to object
; return true/false
(define (object_collision? player object)
  (let* ([walley_left (posn-x player)]
         [walley_right (+ walley_left 110)]
         [walley_top (posn-y player)]
         [walley_bottom (+ walley_top 75)]
         [wider (if(shark? object) (if(>= (shark-difficulty object) 10) 1.7 1) 1)] 
         [object_left (if(shark? object) (posn-x (shark-p object)) (posn-x object))]
         [object_right (if(shark? object) (+ object_left (* wider 50)) (+ object_left 15))]
         [object_top (if(shark? object) (posn-y (shark-p object)) (posn-y object))]
         [object_bottom (if(shark? object) (+ object_top 30) (+ object_top 15))])
    (and (or (>= walley_right object_right walley_left)
         (<= walley_right object_left walley_left))
         (or (>= walley_top object_top walley_bottom)
         (<= walley_top object_bottom walley_bottom)))))

(define (shark_collision? player-pos list-of-sharks)
  (foldl (lambda (shark result) (or (object_collision? player-pos shark) result))
         #f
         list-of-sharks))
                               

;;**************
;;  Enemy Struct
;;**************
;;define/contract struct for enemy shark.
;;
(define-struct/contract shark ([state (or/c 'killing 'dead)]
                               [p (or/c #f posn?)]
                               [difficulty (and/c natural-number/c (<=/c 1000))]
                               [speed (and/c natural-number/c (<=/c 10))]))

;;draw-enemies function
;takes world as arg
;returns list of shark objects

(define (draw-enemies diff-level stage-num time)
    (let* ([speed (cond ((zero? (remainder stage-num 3)) 4)
                       ((= (remainder stage-num 3) 1) 3)
                       (else 2))]
          [number (cond ((<= stage-num 3) 2)
                       ((<= stage-num 6) 3)
                       (else 4))]
          [switch -1]) 
    (map (lambda (p) (make-shark 'killing p diff-level speed))
         (for/list ([i (in-range 0 (* 3 number))])
           (let* ([q (quotient i 3)]
                  [y-offset (* (+ 1 q) 75)])
           (cond ((> 3 i) (make-posn (scroll-left (+ (* 200 i) time)) (- (posn-y START) (* (+ 1 q) 75)))) ;first row
                 ((> 6 i) (make-posn (scroll-left (+ (* 250 i) time)) (- (posn-y START) (* (+ 2 q) 75)))) ;third row
                 ((> 9 i) (make-posn (scroll-right (+ time (* 200 i))) (- (posn-y START) (* q 75)))); middle row
                 (else (make-posn (scroll-right (+ time (* 300 i))) (- (posn-y START) 0))))))))) ;bottom

;;redraw-enemies function
; draws enemies(sharks) based on previous shark-state
; takes world state as arg
;returns list of shark objects
(define (re-draw-enemies s)
    (let* ([time (world-time s)]
           [difficulty_level (world-difficulty s)]
           [stage_n (stage_number difficulty_level)]
           [speed (cond ((zero? (remainder stage_n 3)) 4)
                       ((= (remainder stage_n 3) 1) 3)
                       (else 2))]
           [enemies (stage-Enemies (world-stage s))]
           [number (cond ((<= stage_n 3) 2)
                         ((<= stage_n 6) 3)
                         (else 4))]
          [switch -1])              ;;(make-posn (+ (scroll-right time) (posn-x p)) (posn-y p)
    (map (lambda (s p) (make-shark s p difficulty_level speed))
         (for/list ([i enemies])
           (shark-state i))
         (for/list ([i (in-range 0 (* 3 number))])
           (let* ([q (quotient i 3)]
                  [y-offset (* (+ 1 q) 75)])
           (cond ((> 3 i) (make-posn (scroll-left (+ (* 200 i) time)) (- (posn-y START) (* (+ 1 q) 75)))) ;first row
                 ((> 6 i) (make-posn (scroll-left (+ (* 250 i) time)) (- (posn-y START) (* (+ 2 q) 75)))) ;third row
                 ((> 9 i) (make-posn (scroll-right (+ time (* 200 i))) (- (posn-y START) (* q 75)))); middle row
                 (else (make-posn (scroll-right (+ time (* 300 i))) (- (posn-y START) 0))))))))) ;bottom



;;********************
; Super-Powers Struct
;*********************
(define-struct/contract super-powers ([state  (or/c #t #f)]
                                      [armor-pos (or/c #f posn?)]
                                      [fish-pos (or/c #f posn?)]))

;;function to make-world where everything is the same, but the fish is gone
;;add 1 live to player lives
;;returns a world
(define (found-fish s)
  (let* ([game-state (world-state s)]
         [stage_n (stage_number (world-difficulty s))]
         [difficulty_level (world-difficulty s)]
         [time (world-time s)]
         [player (world-player s)]
         [score (world-score s)]
         [stage (world-stage s)]
         [super-powers (stage-super-powers stage)])
    (make-world game-state time
                (make-player (player-state player) (player-position player) (player-direction player) (add1 (player-lives player)))
                difficulty_level score
                (make-stage game-state difficulty_level (stage-Enemies stage) (stage-Walley stage) (stage-HUD stage) (stage-board stage)
                            (make-super-powers #t (super-powers-armor-pos super-powers) #f))
                (world-inputHandler s))))

;;function to make-world where everything is the same, but the super-horn is gone
;;changes walley to wearing the super-horn
;; changes state of game to 'super?
;;returns a world
(define (found-armor s)
  (let* ([game-state (world-state s)]
         [stage_n (stage_number (world-difficulty s))]
         [difficulty_level (world-difficulty s)]
         [time (world-time s)]
         [player (world-player s)]
         [score (world-score s)]
         [stage (world-stage s)]
         [super-powers (stage-super-powers stage)])
    (make-world game-state time
                (make-player 'super (player-position player) (player-direction player) (player-lives player))
                difficulty_level score
                (make-stage game-state difficulty_level (stage-Enemies stage) super-walley (stage-HUD stage) (stage-board stage)
                            (make-super-powers #t #f (super-powers-fish-pos super-powers)))
                (world-inputHandler s))))


;************
; Tile Struct
;************
;;defines struct for tiles
(define-struct/contract tile ([up? boolean?]
                              [position any/c])
  #:transparent)

(define (get-tile t pos s)
  ;(let ((col (/ (posn-x pos) TILE_WIDTH))
   ;     (row (round (/ (- (+ (posn-y pos) TILE_HEIGHT) (tiles-top-y t s)) TILE_HEIGHT)))
    ;    (colPerRow (/ (posn-x WINDOW) TILE_WIDTH)))
    ;(list-ref t (inexact->exact (round (+ (* row colPerRow) col))))))

  (let* ((tiles (build-board (world-difficulty s)))
         (tilesTopY (tiles-top-y tiles s))
         (posY (posn-y pos))
         (rowY (round (- posY tilesTopY)))
         (colPerRow (/ (posn-x WINDOW) TILE_WIDTH))
         (col (quotient (posn-x pos) TILE_WIDTH))
         (row (round (/ rowY TILE_HEIGHT)))
         (tileCount (length tiles))
         (realIndex (+ (* row colPerRow) col)))
    (if (and (>= realIndex 0) (< realIndex tileCount))
        (list-ref tiles (inexact->exact (round realIndex)))
        '())))

(define (tiles-top-y t s)
  (let* ((tilesPerRow (/ (posn-x WINDOW) TILE_WIDTH))
        (rows (/ (length (build-board (world-difficulty s))) tilesPerRow))
        (gridHeight (* rows TILE_HEIGHT)))
    (- (- (posn-y WINDOW) (/ TILE_HEIGHT 2)) gridHeight)))

;;function to place tiles on the background of the stage
; takes list of #t #f values to determine if a spout_tile or Floor_tile
; returns the image
(define (place-tiles t)
  (map (lambda (n) (if(tile-up? n) SPOUT_TILE FLOOR_TILE))
       t))

;*************
; Build Boards
;*************
; functions to build board of tiles for background of game-play
;spout list
;takes stage_n NOT difficulty as arg
;returns list of integers
(define (spout-list d)
  (let ([l (stage_number d)])
(cond ((eq? l 1) (list 14 19 21 25 32 40 46))
      ((eq? l 2) (list 17 21 28 35 41 46))
      ((eq? l 3) (list 19 25 30 39 45))
      ((eq? l 4) (list 15 24 26 33 37 42 48))
      ((eq? l 5) (list 13 17 25 33 37 44 45 46))
      ((eq? l 6) (list 12 18 23 36 45))
      ((eq? l 7) (list 16 21 29 35 47))
      ((eq? l 8) (list 13 25 37 49))
      ((eq? l 9) (list 14 18 21 29 33 46))
      (else (list 14 18 24 29)))))

;;recursive function that takes an x(width) and y value to fill with positions for tiles.
;;makes a posn and appends to a list, calls func again
(define (make-wide x y l)
  (if (eq? 0 x) l
      (make-wide (- x TILE_WIDTH) y (append (list (make-posn (- x (quotient TILE_WIDTH 2)) (+ y (quotient TILE_HEIGHT 2)))) l))))

(define (build-posn-list x y l)
  (if (< y TILE_HEIGHT) l
      (build-posn-list x (- y TILE_HEIGHT) (make-wide x (- y TILE_HEIGHT) l))))

;tile-posn-list function
;builds list based on difficulty and window size
;return list of positions of tiles
(define (tile-posn-list d)
  (build-posn-list (posn-x WINDOW) (- (posn-y WINDOW) TILE_HEIGHT) '()))


;;builds a list of tiles based on stage list
;makes list of t/f the size of x*y
;makes list of positions based on x*y and window size
;uses map to create list of tile objects
(define (build-board diff-level)
  (let* ([x (/ (posn-x WINDOW) TILE_WIDTH)]
         [y (quotient (- (posn-y WINDOW) TILE_HEIGHT) TILE_HEIGHT)])
    (map (lambda (b p) (make-tile b p))
         (for/list ([i (in-range 0 (* x y))])
           (if (member i (spout-list diff-level)) #t #f))
         (tile-posn-list diff-level))))

;************
; HUD
;************
;;displays important player info: lives left, score, stage number,
;@Jacob-Phillips : possible a time counter?
;;
(define HUD-AREA (rectangle (posn-x WINDOW) 50 "solid" CLEAR))
;;(define HEART (circle 5 "solid" "pink"))
(define HEART (freeze (scale 1/6 happy-walley)))
;;returns scene with info loaded on HUD-AREA
(define (draw-HUD lives diff sc)
  ;;create list of component and positions
  (define hud-comp
   (append (for/list ([i (in-range 0 lives)])
                HEART)
           (list (text "STAGE:" 22 "black")
                (text (number->string (stage_number diff)) 22 "black")
               (cond
                     ((< diff 10) NORMAL)
                     ((< diff 100) LASER-SHARKS)
                    ((< diff 1000) SHARKNADO))                      
                   (text "SCORE:" 14 "Black")
                   (text (number->string sc) 14 "black"))))
  (define hud-posn
    (append (for/list ([i (in-range 1 (+ lives 1))])
               (make-posn (* i 30) 20))
            (list (make-posn 200 20) ;stage text
                  (make-posn 250 20) ;stage num
                  (make-posn 250 40) ; diff setting
                  (make-posn 375 20) ;score text
                  (make-posn 425 20)))) ;player score
  (place-images  hud-comp hud-posn HUD-AREA))

;*************
; Stage Struct
;*************
;;defines and contracts the player object
;;needs to be expanded to include player characteristics
;;this definition type automatically creates:
;  constructor= (make-stage state dif list-of-enemies Walley HUD Board superpowers)
;  eq test= stage?
;  fields= stage-state, stage-difficulty, stage-Enemies, stage-Walley, stage-HUD, stage-board, stage-super-powers
(define-struct/contract stage ([state (or/c 'start
                                            'playing
                                            'dead
                                            'won
                                            'lost)]
                               [difficulty (and/c natural-number/c (<=/c 1000))]
                               [Enemies (or/c #f (listof shark?))]
                               [Walley any/c]
                               [HUD any/c]
                               [board (or/c #f (listof tile?))]
                               [super-powers (or/c #f super-powers?)]) ;;added)
  #:transparent)

;;BEHOLD-Stage function
; called on-draw of big-bang
; takes world as arg and splits into usable info to build stage/render images for game-play
; two states of 'start and 'playing
; modifies stage fields as neccesary to facilite game functions
; **returns IMAGE object** NOT world state
(define (BEHOLD-Stage s)
  (cond ((eq? 'start (world-state s))  ;;creates original stage
         (let* ([stage_n (stage_number (world-difficulty s))]
                [difficulty_level (world-difficulty s)]
                [time (world-time s)]
                [player (world-player s)]
                [score (world-score s)]
                [sp (make-super-powers (super-powers-state (stage-super-powers (world-stage s)))
                                       (if (< stage_n 5) #f (super-powers-armor-pos (stage-super-powers (world-stage s))))
                                       (if (and (>= stage_n 5) (< (player-lives player) 5)) (super-powers-fish-pos (stage-super-powers (world-stage s))) #f))]
                [st (make-stage 'playing difficulty_level (draw-enemies difficulty_level stage_n time)
                                (swim (player-state player) (player-direction player) time)
                                (draw-HUD (player-lives player) difficulty_level score)
                                (build-board difficulty_level) sp)]
                [stage-comp (append (list (scale (if(eq? (player-state player) 'super)
                                                    5/9
                                                    1/2)
                                                 (swim (player-state player) (player-direction player) time))
                                          (rainbow time)
                                          (stage-HUD st))
                                    (if(super-powers-state sp) (list (if(super-powers-armor-pos sp) (scale 2/3 (super-horn time)) '())
                                                                     (if(super-powers-fish-pos sp) (scale 2/3 fish) '())) '())
                                    (place-tiles (stage-board st))
                                    (for/list ([i (length (stage-Enemies st))])
                                      (cond ((< difficulty_level 10) (if(< i 6) (flip-horizontal sharkfin) sharkfin))
                                            ((< difficulty_level 100) (if(< i 6) (flip-horizontal laser-shark) laser-shark))
                                            ((< difficulty_level 1000) (if(< i 6) (flip-horizontal laser-shark)sharkfin)))))]
                [stage-posn (append (list (player-position player) (make-posn 50 75) (make-posn (quotient (posn-x WINDOW) 2) 18))
                                    (if(super-powers-state sp) (list (super-powers-armor-pos sp) (super-powers-fish-pos sp)) '())
                                    (tile-posn-list stage_n)
                                    (for/list ([i (stage-Enemies st)])
                                      (shark-p i)))])
           ;;draw stage components and test for sharknado animation
           (cond ((>= difficulty_level 100) (create-sharknado time (place-images (filter (lambda (e) (not (empty? e))) stage-comp) (filter (lambda (e) (not (eq? #f e))) stage-posn) BACKGROUND))) ;Sharknado activation
                 (else (place-images (filter (lambda (e) (not (empty? e))) stage-comp) (filter (lambda (e) (not (eq? #f e))) stage-posn) BACKGROUND)))))
         ((eq? 'playing (world-state s)) ;;re-draws stage after movement
          (let* ([stage_n (stage_number (world-difficulty s))]
                [difficulty_level (world-difficulty s)]
                [time (world-time s)]
                [player (world-player s)]
                [score (world-score s)]
                [sp (make-super-powers (super-powers-state (stage-super-powers (world-stage s)))
                                       (if (< stage_n 5) #f (super-powers-armor-pos (stage-super-powers (world-stage s))))
                                       (if (and (>= stage_n 5) (< (player-lives player) 5)) (super-powers-fish-pos (stage-super-powers (world-stage s))) #f))]
                [st (make-stage 'playing difficulty_level (re-draw-enemies s)
                                (swim (player-state player) (player-direction player) time)
                                (draw-HUD (player-lives player) difficulty_level score)
                                (build-board difficulty_level) sp)]
                [stage-comp 
                 (append (list (scale (if(eq? (player-state player) 'super)
                                                    5/9
                                                    1/2)
                                                 (swim (player-state player) (player-direction player) time))
                               (rainbow time)
                               (stage-HUD st))
                         (if(super-powers-state sp) (list (if(posn? (super-powers-armor-pos sp)) (scale 2/3 (super-horn time)) '()) (if(posn? (super-powers-fish-pos sp)) (scale 2/3 fish) '())) '())
                         (place-tiles (stage-board st))
                         (for/list ([i (length (stage-Enemies st))])
                                      (cond ((< difficulty_level 10) (if(< i 6) (flip-horizontal sharkfin) sharkfin))
                                            ((< difficulty_level 100) (if(< i 6) (flip-horizontal laser-shark) laser-shark))
                                            ((< difficulty_level 1000) (if(< i 6) (flip-horizontal laser-shark) sharkfin)))))]
                [stage-posn
                 (append (list (player-position player) (make-posn 50 75) (make-posn (quotient (posn-x WINDOW) 2) 18))
                         (if(super-powers-state sp) (list (super-powers-armor-pos sp) (super-powers-fish-pos sp)) '())
                         (tile-posn-list stage_n)
                         (for/list ([i (stage-Enemies st)])
                           (shark-p i)))]) ;end let*
         ;;draw stage components
           (cond ((>= difficulty_level 100) (create-sharknado time (place-images (filter (lambda (e) (not (empty? e))) stage-comp) (filter (lambda (e) (not (eq? #f e))) stage-posn) BACKGROUND))) ;;sharknado animation
                 (else (place-images (filter (lambda (e) (not (empty? e))) stage-comp) (filter (lambda (e) (not (eq? #f e))) stage-posn) BACKGROUND)))))))

;;function to advance stage
; takes world as args
; changes stage and difficulty, resets all enemies, super-powers and time
;;returns world
(define (advance-stage s)
  (let* ([stage_n (add1 (stage_number (world-difficulty s)))]
         [difficulty_level (next-stage (world-difficulty s))]
         [time 0]
         [player (make-player 'swimming START "left" (player-lives (world-player s)))]
         [score (world-score s)]
         [horn-x (random 2 8)]
         [fish-x (random 2 8)]
         [horn-y (random 1 4)]
         [fish-y (random 1 4)]
         [sp (make-super-powers (if (>= stage_n 5) #t #f)
                                (if (>= stage_n 5) (make-posn (- 500 (* 50 horn-x)) (- 390 (* 75 horn-y))) #f)
                                (if (>= stage_n 5) (make-posn (- 500 (* 50 fish-x)) (- 390 (* 75 fish-y))) #f))]
         [stage (make-stage 'start difficulty_level (draw-enemies difficulty_level stage_n time)
                         (swim (player-state player) (player-direction player) time)
                         (draw-HUD (player-lives player) difficulty_level score)
                         (build-board difficulty_level) sp)])
  (make-world 'start 0 player difficulty_level score stage (world-inputHandler s))))

;;function to return stage number from difficulty level
(define (stage_number d)
  (cond
    ((< d 10) d)
    ((< d 100) (/ d 10))
    ((< d 1000) (/ d 100))))
;;function for next stage math
(define (next-stage d)
  (cond
    ((< d 10) (+ d 1))
    ((< d 100) (+ d 10))
    ((< d 1000) (+ d 100))))
;;function to determine stage from difficulty
(define (difficulty-from-stage d)
  (cond
    ((< d 10) 1)   ; [1,9]      => 1   (easy)
    ((< d 100) 10) ; [10, 99]   => 10  (medium)
    (else 100)))   ; [100, 999] => 100 (hard)
;;function to revert to first stage
(define (stage-reset d) (difficulty-from-stage d))

;**************
;INPUT HANDLERS
;**************
(define paused-input-handler
   (lambda (wrld input)
    (cond ((or (key=? input "c") (key=? input "C")) (make-world 'playing (world-time wrld) (world-player wrld) (world-difficulty wrld) (world-score wrld) (world-stage wrld) playing-input-handler))
          ((or (key=? input "q") (key=? input "Q")) (make-world 'splash_screen 0 (make-player 'swimming START "left" 3) (stage-reset (world-difficulty wrld)) 0 (world-stage wrld) splash-input-handler))
          (else wrld))))

(define playing-input-handler
   (lambda (wrld input)
     (let* ([game-state (world-state wrld)]
         [stage_n (stage_number (world-difficulty wrld))]
         [difficulty_level (world-difficulty wrld)]
         [time (world-time wrld)]
         [player (world-player wrld)]
         [score (world-score wrld)]
         [stage (world-stage wrld)]
         [super-powers (stage-super-powers stage)])
     (cond ((object_collision? (player-position player) (make-posn 40 100)) (advance-stage wrld))
           ((and (super-powers-fish-pos super-powers) (object_collision? (player-position player) (super-powers-fish-pos super-powers))) (found-fish wrld))
           ((and (super-powers-armor-pos super-powers) (object_collision? (player-position player) (super-powers-armor-pos super-powers))) (found-armor wrld))
           ((and (eq? (player-state player) 'swimming) (shark_collision? (player-position player) (stage-Enemies stage)))
            (if (= (player-lives player) 0) (make-world 'lost  0 player difficulty_level score stage gameover-input-handler)
                (make-world 'start 0 (make-player (player-state player) START "left" (- (player-lives player) 1))
                            difficulty_level score
                            (make-stage 'start 1 (re-draw-enemies wrld) happy-walley (draw-HUD (player-lives player) stage_n score) (build-board stage_n) super-powers)
                            (world-inputHandler wrld))))
           ((pad=? input "rshift") (make-world 'paused 0 player difficulty_level score stage paused-input-handler))
           ((pad=? input "shift")  (make-world 'paused 0 player difficulty_level score stage paused-input-handler))
           ((and (pad=? input "right")(not (player-climbing-spout wrld)))  (make-world 'playing time (move-player wrld "right") difficulty_level score stage (world-inputHandler wrld)))
           ((and (pad=? input "left") (not (player-climbing-spout wrld))) (make-world 'playing time (move-player wrld "left")  difficulty_level score stage (world-inputHandler wrld)))
           ((pad=? input "up")     (make-world 'playing time (move-player wrld "up")    difficulty_level score stage (world-inputHandler wrld)))
           ((pad=? input "d")      (make-world 'playing time (move-player wrld "right") difficulty_level score stage (world-inputHandler wrld)))
           ((pad=? input "a")      (make-world 'playing time (move-player wrld "left")  difficulty_level score stage (world-inputHandler wrld)))
           
           ;;test for win/stage prog- w for win-test, s for die test
           ((and (pad=? input "w") (= difficulty_level 900)
                           (make-world 'won 0 player 1 score stage gameover-input-handler)))
           ((pad=? input "w") (advance-stage wrld))
           ;;put player back at Start, decrease lives
           ((and (pad=? input "s") (= (player-lives player) 0))
                           (make-world 'lost  0 player difficulty_level score stage gameover-input-handler))
           ((pad=? input "s") (make-world 'start 0 (make-player (player-state player) START "left" (- (player-lives player) 1))
                                       difficulty_level score stage (world-inputHandler wrld)))
           (else wrld)))))

(define splash-input-handler
   (lambda (wrld input)
     (let* ([game-state (world-state wrld)]
         [stage_n (stage_number (world-difficulty wrld))]
         [difficulty_level (world-difficulty wrld)]
         [time (world-time wrld)]
         [player (world-player wrld)]
         [score (world-score wrld)]
         [stage (world-stage wrld)]
         [super-powers (stage-super-powers stage)])
       (cond
         ((pad=? input " ") (make-world 'start 0 (make-player 'swimming START "left" 3) (world-difficulty wrld) 0
                                             (make-stage 'start 1
                                                        (draw-enemies (world-difficulty wrld) 1 1)
                                                        happy-walley
                                                        (draw-HUD 3 (world-difficulty wrld) 0)
                                                        (build-board (world-difficulty wrld))
                                                        (make-super-powers #f #f #f))
                                                        playing-input-handler))
         ((pad=? input "rshift") (make-world 'help_screen 0 player (world-difficulty wrld) score stage help-input-handler))
         ((pad=? input "shift")  (make-world 'help_screen 0 player (world-difficulty wrld) score stage help-input-handler))
         ((pad=? input "up")   (make-world-decrease-difficulty wrld)) ; invert up/down to match scroll direction
         ((pad=? input "down") (make-world-increase-difficulty wrld)) ; invert up/down to match scroll direction
         (else wrld)))))

(define help-input-handler
   (lambda (wrld input)
     (let* ([game-state (world-state wrld)]
         [stage_n (stage_number (world-difficulty wrld))]
         [difficulty_level (world-difficulty wrld)]
         [time (world-time wrld)]
         [player (world-player wrld)]
         [score (world-score wrld)]
         [stage (world-stage wrld)]
         [super-powers (stage-super-powers stage)])
       (cond
    ((equal? game-state 'help_screen)
     (cond ((pad=? input "rshift") (make-world 'splash_screen 0 player difficulty_level score stage splash-input-handler))
           ((pad=? input "shift")  (make-world 'splash_screen 0 player difficulty_level score stage splash-input-handler))
           ((pad=? input "right")  (make-world 'help_screen_2 0 player difficulty_level score stage help-input-handler))
           (else wrld)))
    ((equal? game-state 'help_screen_2)
     (cond ((pad=? input "rshift") (make-world 'splash_screen 0 player difficulty_level score stage splash-input-handler))
           ((pad=? input "shift")  (make-world 'splash_screen 0 player difficulty_level score stage splash-input-handler))
           ((pad=? input "left")   (make-world 'help_screen   0 player difficulty_level score stage help-input-handler))
           (else wrld)))))))

(define gameover-input-handler
   (lambda (wrld input)
     (let* ([game-state (world-state wrld)]
         [stage_n (stage_number (world-difficulty wrld))]
         [difficulty_level (world-difficulty wrld)]
         [time (world-time wrld)]
         [player (world-player wrld)]
         [score (world-score wrld)]
         [stage (world-stage wrld)]
         [super-powers (stage-super-powers stage)])
     (cond ((pad=? input " ")      (make-world 'playing 0 (make-player 'swimming START "left" 3) (stage-reset difficulty_level) 0 stage playing-input-handler))
           ((pad=? input "rshift") (make-world 'help_screen 0 player difficulty_level score stage help-input-handler))
           ((pad=? input "shift")  (make-world 'help_screen 0 player difficulty_level score stage help-input-handler))
           ((pad=? input "up")     (make-world-decrease-difficulty wrld)) ; invert up/down to match scroll direction
           ((pad=? input "down")   (make-world-increase-difficulty wrld)) ; invert up/down to match scroll direction
           (else wrld)))))
  
;************
;WORLD-STRUCT
;************
;;defines and contracts the world object
;;needs to be expanded to include: stage-board(board object), player(player object), player-posn(posn)
;;this definition type automatically creates:
;  constructor= (make-world state time player difficulty score stage inputHandler)
;  eq test= world?
;  fields= world-state, world-player, world-time, world-difficulty, world-score, world-stage, world-inputHandler

(define-struct/contract world ([state (or/c 'splash_screen
                                            'start
                                            'playing
                                            'paused
                                            'won
                                            'lost
                                            'help_screen
                                            'help_screen_2)]
                               [time (and/c natural-number/c (<=/c 100000))]
                               [player (or/c #f player?)]
                               [difficulty (and/c natural-number/c (<=/c 1000))]
                               [score (and/c natural-number/c (>=/c 0))]
                               [stage (or/c #f stage?)]
                               [inputHandler (-> any/c (or/c pad-event? key-event?) any/c)]) ;cannot specify world? in world structure declaration
  #:transparent)

;;advance time function
; called on-tick of big-bang
;used to count time, animate objects
;takes world as arg
; increases time count, passes changes to stage **otherwise drawn object positions don't change**
; returns world object
(define (advance-time s)
  (let* ([stage_n (stage_number (world-difficulty s))]
         [difficulty_level (world-difficulty s)]
         [time (add1 (world-time s))]
         [player (world-player s)]
         [score (world-score s)]
         [sp (stage-super-powers (world-stage s))]
         [stage (make-stage 'start difficulty_level (draw-enemies difficulty_level stage_n time)
                         (swim (player-state player) (player-direction player) time)
                         (draw-HUD (player-lives player) difficulty_level score)
                         (build-board difficulty_level) sp)]
         [handler (world-inputHandler s)])
  (cond ((not (eq? (world-state s) 'playing))
         (make-world (world-state s) (if (= 500 (world-time s)) 0 time) player difficulty_level score stage handler))
        (else (make-world 'playing time player difficulty_level score
                          (make-stage 'playing difficulty_level (re-draw-enemies s)
                                (swim (player-state player) (player-direction player) time)
                                (draw-HUD (player-lives player) difficulty_level score)
                                (build-board difficulty_level) sp) handler)))))

(define (make-world-increase-difficulty s)
        (let* ((current (difficulty-from-stage (world-difficulty s)))
               (updated (cond ((eq? current 1) 10)
                              ((eq? current 10) 100)
                              (else 1))))
              (make-world (world-state s) (world-time s) (world-player s) updated (world-score s) (world-stage s) (world-inputHandler s))))

(define (make-world-decrease-difficulty s)
        (let* ((current (difficulty-from-stage (world-difficulty s)))
               (updated (cond ((eq? current 1) 100) ; wrap easy => hard
                              ((eq? current 10) 1)
                              (else 10))))
              (make-world (world-state s) (world-time s) (world-player s) updated (world-score s) (world-stage s) (world-inputHandler s))))

;;make worlds
;;make-initial-world(create the splash_screen)
;; creates defualt world-state for initial state of big-bang
; sets all values to 1 or defaults
(define (make-water-world)
  (make-world 'splash_screen
              0
              (make-player 'swimming START "left" 3)
              1
              0
              (make-stage 'start
                          1
                          (draw-enemies 1 1 1)
                          happy-walley
                          (draw-HUD 3 1 0)
                          (build-board 1)
                          (make-super-powers #f #f #f))
              splash-input-handler))

;;render-world
;;draws the world based on the state of the world
;;must take a world-state as an arg because it is called in 'big-bang'
;;must return a scene or image object
(define (render-world s)
  (cond
    ((equal? (world-state s) 'splash_screen) (render-splashscreen s))
    ((equal? (world-state s) 'help_screen) (render-helpscreen s))
    ((equal? (world-state s) 'help_screen_2) (render-helpscreen s))
    ((equal? (world-state s) 'playing) (BEHOLD-Stage s))
    ((equal? (world-state s) 'start) (BEHOLD-Stage s))
    ((equal? (world-state s) 'paused) (render-pausedscreen s))
    ((equal? (world-state s) 'lost) (render-splashscreen s))
    (else (render-splashscreen s))))


;*************
; MENU display
;*************
;BEHOLD-Menu- contains list of menu components, list of menu posns
;places components at posns in order to display info to user
;; takes world as arg and splits the info from it
;;returns image object
(define (render-splashscreen s)
   (place-images
     (list
      (text "Pelagic-Kong" 40 "cyan")
      (place-image (text (cond
                           ((eq? (world-state s) 'lost) "GAME OVER")
                           ((eq? (world-state s) 'won) "CONGRATULATIONS!")
                           (else "NEW GAME"))
                         18 "Yellow")
                   200 20
                   (place-image (beside (text "SCORE:  " 16 "blue")
                                        (text (number->string (world-score s)) 18 "blue"))
                                200 40
                               (place-image (text "Change difficulty or Press SPACE to start" 16 "blue")
                                            200 70
                                            (rectangle 400 100 "outline" CLEAR))))
      (text "DIFFICULTY:" 18 "black")
      NORMAL
      LASER-SHARKS
      SHARKNADO
      HIGHLIGHT
      (text "Press shift for help" 16 "Blue")
     (scale 3/4 (draw-walley 'swimming (world-time s)))
     (scale 2 (overlay/align "middle" "bottom" (crop 0 25 35 50 FLOOR_TILE) (crop 0 25 25 50 sharkfin))))
     (list (make-posn 250 20) ;title
           (make-posn 250 110) ;game status box
           (make-posn 250 160);diff
           (make-posn 250 175);n
           (make-posn 250 190);l
           (make-posn 250 205);sharknado
           (highlight_difficulty (world-difficulty s))
           (make-posn 250 235);help text
           (make-posn (scroll-right (world-time s)) 320)
           (make-posn (scroll-right (- (world-time s) 150)) 350))
     BACKGROUND))

;;function to highlight difficulty level
(define (highlight_difficulty d)
  (let* ((difficulty (difficulty-from-stage d))
         (pos-y (cond ((eq? difficulty 1) 175)
                      ((eq? difficulty 10) 190)
                      ((eq? difficulty 100) 205))))
        (make-posn 250 pos-y)))


;***********
; HELP Screen
;***********
(define (render-helpscreen s)
  (cond ((eq? (world-state s) 'help_screen)
              (place-image
               (above/align "middle"
                            (text "Pelagic-Kong" 30 "Cyan")
                            (rectangle 50 30 "outline" CLEAR)
                            (beside
                             (text "Use the  " 17 "red")
                             (scale 1.5 (key 'left))
                             (text "  and " 17 "RED")
                             (scale 1.5 (key 'right))
                             (text "  to make " 17 "RED")
                             (scale 1/3 happy-walley)
                             (text "  swim left and right." 17 "RED")
                             )
                            (beside
                             (text "Reach the  " 17 "red")
                             (scale 1/2 (rainbow (world-time s)))
                             (text "  without getting eaten by a " 17 "RED")
                             (scale 1 (overlay/align "middle" "bottom" (crop 0 25 35 50 FLOOR_TILE) (crop 0 25 25 50 sharkfin))))
                            (beside
                              (text "Use the " 17 "RED")
                              (scale 1.5 (key 'up))
                             (text "  and " 17 "RED")
                             (scale 1.5 (key 'down))
                             (text "  to make " 17 "RED")
                             (scale 1/3 happy-walley)
                             (text " swim up & down the " 17 "RED")
                             (scale 1/2 SPOUT_TILE))
                            (beside
                              (text "Catching a " 17 "RED")
                             (scale 1/2 (rotate -30 fish))
                             (text "  will give you an extra life." 17 "RED")
                             (rectangle 50 50 "outline" CLEAR))
                            (beside
                             (text "Finding the " 17 "RED")
                             (scale 2/3 (super-horn (world-time s)))
                             (text "  will protect you from the " 17 "RED")
                             (scale 1 (overlay/align "middle" "bottom" (crop 0 25 35 50 FLOOR_TILE) (crop 0 25 25 50 sharkfin))))
                            (rectangle 50 30 "outline" CLEAR)
                            (beside
                             (text "Press" 18 "RED")
                              (scale 1.5(key 'right))
                               (text "for next page" 18 "RED"))
                            (rectangle 50 30 "outline" CLEAR)
                            (text "Press SHIFT to return to Splash Screen" 20 "RED"))
                            
               250 220
               BACKGROUND))
         (else (place-image
               (above/align "middle"
                            (text "Pelagic-Kong" 30 "Cyan")
                            (beside
                            (text "Difficulty Options:" 16 "yellow")
                            (rectangle 50 40 "outline" CLEAR))
                            (beside
                             (text "NORMAL: " 16 "black")
                            (scale 1 (overlay/align "middle" "bottom" (crop 0 25 35 50 FLOOR_TILE) (crop 0 25 25 50 sharkfin))))
                             (text " Will kill you if you get too close" 16 "RED")
                            (beside
                             (text "SHARKS-WITH-LASERBEAMS: " 16 "black")
                            (scale 1 (overlay/align "left" "bottom" (crop 0 25 35 50 FLOOR_TILE) (crop 0 25 50 50 laser-shark))))
                             (text " Can kill you from farther away" 16 "RED")
                             (beside
                             (text "SHARK-NADO: " 16 "black")
                            (scale 1/4 sharknado_still))
                             (text " Randomly drops random sharks from the sky, randomly" 16 "RED")
                            (beside
                             (text "Armored Horn " 16 "yellow")
                             (scale 4/5 (super-horn (world-time s)))
                             (text " will protect you from any kind of shark" 16 "red"))
                             (rectangle 50 10 "outline" CLEAR)
                             (beside 
                            (text "Change the difficulty with the " 16 "RED")
                            (scale 1.5 (key 'up))
                             (text "  and " 17 "RED")
                             (scale 1.5 (key 'down))
                             (text "  on the splash-screen." 17 "RED"))
                            (rectangle 50 10 "outline" CLEAR)
                            (beside
                             (text "Press" 18 "RED")
                              (scale 1.5(key 'left))
                               (text "for previous page" 18 "RED"))
                            (text "Press SHIFT to return to Splash Screen" 18 "RED"))
               250 220
               BACKGROUND))))

;**************
; Paused screen
;**************
;Allows for continue or restart
(define (render-pausedscreen s)
  (let ([player (world-player s)])
   (place-images
     (list
      (text "Pelagic-Kong" 40 "cyan")
      (place-image (text "PAUSED" 18 "Yellow")
                   200 20
                   (place-image (beside (text "Score:  " 16 "blue")
                                        (text (number->string (world-score s)) 18 "blue"))
                                200 40
                                (place-image (beside (text "Lives remaining: " 16 "blue")
                                                     (text (number->string (player-lives player)) 18 "blue"))      
                                            200 70
                                            (rectangle 400 100 "outline" CLEAR))))
      (text "Press c to continue or q to quit." 18 "black")
      (draw-walley 'swimming (world-time s))
      )
     (list (make-posn 250 20) ;title
           (make-posn 250 110) ;game status box
           (make-posn 250 200);continue/quit
           (make-posn (scroll-right (world-time s)) 320))
     
     BACKGROUND)))


;**************
;Key handler
;**************
;change handles keyboard input from user
(define (keyboard-handler s ke) ((world-inputHandler s) s ke))

;**************
;Pad handler
;**************
(define (pad-handler s pe) ((world-inputHandler s) s pe))

;;***********
;;main driver
;;***********
;;defines main funtion(no args)
; calls big-bang with initial state of (make-water-world) which initially creates the splash_screen
; each clause in big-bang must take the state of the world as an arg, and returns a new state
;starts game by calling make-world with new state
  (define (main)
  (big-bang (make-water-world)
    [on-tick advance-time]
    [to-draw render-world]
    [name "Pelagic-Kong"]
    [on-key keyboard-handler]
    [on-pad pad-handler]
    [close-on-stop #t]))

(main)
