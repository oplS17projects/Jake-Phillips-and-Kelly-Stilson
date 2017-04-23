#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)


;************
; CONSTANTS
;************
;;defines constants used in the making of the worlds
;;need to be changed to functions to build the actually SPLASH_SCREEN, Game-stage, etc


(define WINDOW (make-posn 500 500))
(define TILE_HEIGHT 75)
(define TILE_WIDTH 50)
(define START (make-posn (- (posn-x WINDOW) 50) (- (posn-y WINDOW) (* TILE_HEIGHT 1.5))))

(define CLEAR (make-color 100 100 100 0))
(define BACKGROUND (square (posn-x WINDOW) "solid" "Medium Aquamarine"))
(define NORMAL (text "NORMAL" 14 "RED"))
(define LASER-SHARKS (text "SHARKS-WITH-LASERBEAMS" 14 "RED"))
(define HIGHLIGHT (rectangle 200 15 "solid" "yellow"))
(define SHARKNADO (text "SHARK-NADO" 14 "RED"))


;*************
;Images Defined
;*************
(define SEAFOAM (make-color 104 220 171 85))
;;(define SEAFOAM "CadetBlue")

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
                 23 1
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
   2 32
   ;stem
   (polygon (list (make-pulled-point 1/4 60 15 10 1/4 -60)
                  (make-pulled-point 1/4 0 18 60 1/2 0)
                  (make-pulled-point 1/2 0 12 60 1/2 0))
            "solid" "LightSeaGreen"))))

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


;;Shark
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
;;laser-shark
(define laser-shark
  (add-line
   (add-line sharkfin
             5 50
             18 50
             (make-pen "black" 4 "solid" "round" "round"))
            20 50
            50 50
            "orange"))

;;*****SHARKNADO*****
(define light-wind (make-color 0 158 158 200))
(define dark-wind (make-color 0 133 133 250))
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
(define sharknado
  (overlay
   (rotate 3 sharkfin)
   (draw-sharknado (ellipse 5 10 "solid" light-wind) 30)
   BACKGROUND))

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

;;Super-Powers

(define ping
  (pulled-regular-polygon 10 4 3/4 25 "solid" "Ghostwhite"))

(define super-horn (scale 1.2 (rotate -30
                           (overlay/align/offset "middle" "middle"
                                          ping
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
;;fish
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

;;Rainbow
(define rainbow
  (place-images (list ping (rotate 40 ping) ping)
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
;;Walley Character images and draw-functions
;****************
(define (draw-walley mood t)
   ;;face
  (define happy-face
      (add-line
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
       15 75 0 80 "darkblue"))
  (define sad-face
      (add-line
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
       15 75 0 80 "darkblue"))
  (define face
    (cond
    ((eq? mood 'happy) happy-face)
    ((eq? mood 'swimming) happy-face)
    (else sad-face)))  
  (define eye-r (overlay/offset (circle 5 "solid" "white")
                  -3 1
                  (circle 9 "solid" "RoyalBlue")))
  (define eye-l (overlay/offset (circle 5 "solid" "white")
                  -3 1
                  (circle 9 "solid" "RoyalBlue")))
  (define ping
  (pulled-regular-polygon 10 4 3/4 25 "solid" "Ghostwhite"))




(define horn (rotate -30(scene+curve
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
                     "DarkGoldenrod")))

  ;;create closed polygon using list of posn's(x y), points in clockwise order
  (define tail
   (rotate (cond ((odd? t) 30)
                  (else 60))                                           
            (polygon (list (make-pulled-point 1/2 10 30 10 1/2 -10) ;;middle tail point
                           (make-pulled-point 1/2 45 60 10 3/4 45) ;;right tail point
                           (make-pulled-point 1/2 -10 40 70 1/2 -20) ;;body tail point
                           (make-pulled-point 1/2 -20 0 10 1/2 -45)) ;;left tail point
                     "solid"
                     "CornflowerBlue")))
  (define body
    (polygon (list (make-pulled-point 1/2 45 0 0 1/2 -45) ;;top point
                   (make-pulled-point 1/2 45 40 40 1/2 -45) ;;right middle
                   (make-pulled-point 1/2 45 -20 100 0 0)  ;;bottom middle
                   (make-pulled-point 3/4 10 -70 85 5/6 5) ;;left bottom corner
                   (make-pulled-point 1/2 -20 -40 40 1/2 -45)) ;;left middle
             "solid"
             "CornflowerBlue"))

  (define face-posn
    (make-posn 112 85))
  (define eye-r-posn
    (make-posn 100 70))
  (define eye-l-posn
    (make-posn 145 68))
  (define horn-posn
    (make-posn 120 35))
  (define body-posn
    (make-posn 98 95))
  (define tail-posn
    (make-posn 35 110))

  ;;underlays images in order of list
  (place-images
   (list eye-r eye-l horn face tail body)
   (list eye-r-posn eye-l-posn horn-posn face-posn tail-posn body-posn)
   (rectangle 170 150 "outline" CLEAR)))

;;constants for walley
(define happy-walley
 (draw-walley 'swimming 0))
(define sad-walley
 (draw-walley 'dead 0))

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

(define (move_walley p direction)
  (let ((posn-offset-x (cond ((eq? direction "right") 10)
                             ((eq? direction "left") -10)
                             (else 0)))
        (posn-offset-y (cond ((eq? direction "down") 10)
                             ((eq? direction "up") -10)
                             (else 0))))
       (make-player (player-state p)
                    (posn-offset (player-position p) (make-posn posn-offset-x posn-offset-y))
                    direction
                    (player-lives p))))

;;Funtion to scroll image, only changes x value based on time and speed args
(define (scroll-image t s)
  (modulo (+ t (posn-x WINDOW)) (posn-x WINDOW)))


  ;(cond ((< t 500) (* (- t 20) (ceiling (/ s 2))))
   ;     ((< t 1000) (* (- t 520) (ceiling (/ s 2))))
    ;    ((< t 1500) (* (- t 1020) (ceiling (/ s 2))))
     ;   ((< t 2000) (* (- t 1520) (ceiling (/ s 2))))
      ;  (else (- t 550))))
                  

;;**********
;;  Enemy Struct
;;**********

;;define/contract struct for emeny shark.
;;
(define-struct/contract shark ([state (or/c 'killing 'dead)]
                               [p (or/c #f posn?)]
                               [difficulty (and/c natural-number/c (<=/c 1000))]
                               [speed (and/c natural-number/c (<=/c 10))]))

;;draw-enemies funtion takes the difficulty setting of the level to create a list of shark items
  (define (draw-enemies diff-level stage-num time)
    (let ([speed (cond ((zero? (remainder stage-num 3)) 4)
                       ((= (remainder stage-num 3) 1) 3)
                       (else 2))]
          [number (cond ((<= stage-num 3) 2)
                       ((<= stage-num 6) 3)
                       (else 4))]
          [switch -1]) 
    (map (lambda (p) (make-shark 'killing p diff-level speed))
         (for/list ([i (in-range 0 number)])
           (make-posn (+ (* 50 i) (scroll-image time speed)) (* (+ i 2) (/ (posn-y START) 5)))))))
     
  
(define (screen_shot s)
  (freeze (BEHOLD-Stage 1 (world-player s) (world-score s))))

;***********
; MENU display
;***********
;BEHOLD-Menu- contains list of menu components, list of menu posns
;places components at posns in order to display info to user
;; need to change placeholder text for actual info from player $ world objects
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
     (draw-walley 'swimming (world-time s))
      )
     (list (make-posn 250 20) ;title
           (make-posn 250 110) ;game status box
           (make-posn 250 160);diff
           (make-posn 250 175);n
           (make-posn 250 190);l
           (make-posn 250 205);sharknado
           (highlight_difficulty (world-difficulty s))
           (make-posn 250 235);help text
           (make-posn (scroll-image (world-time s) 2) 320))
     
     BACKGROUND))

;;function to highlight difficulty level
(define (highlight_difficulty d)
  (let* ((difficulty (difficulty-from-stage d))
         (pos-y (cond ((eq? difficulty 1) 175)
                      ((eq? difficulty 10) 190)
                      ((eq? difficulty 100) 205))))
        (make-posn 250 pos-y)))

;***********
; Paused screen
;***********
;Allows for continue or restart
(define (render-pausedscreen s)
   (place-images
     (list
      (text "Pelagic-Kong" 40 "cyan")
      (place-image (text "PAUSED" 18 "Yellow")
                   200 20
                   (place-image (beside (text "Score:  " 16 "blue")
                                        (text (number->string (world-score s)) 18 "blue"))
                                200 40
                                (place-image (beside (text "Lives remaining: " 16 "blue")
                                                     (text (number->string (player-lives (world-player s))) 18 "blue"))      
                                            200 70
                                            (rectangle 400 100 "outline" CLEAR))))
      (text "Press c to continue or q to quit." 18 "black")
      (draw-walley 'swimming (world-time s))
      )
     (list (make-posn 250 20) ;title
           (make-posn 250 110) ;game status box
           (make-posn 250 200);continue/quit
           (make-posn (scroll-image (world-time s) 2) 320))
     
     BACKGROUND))

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
                             (scale 1/2 rainbow)
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
                             (scale 2/3 super-horn)
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
                            (scale 1 (overlay/align "middle" "bottom" (crop 0 25 50 50 FLOOR_TILE) (crop 0 25 50 50 sharkfin))))
                             (text " Randomly drops random sharks from the sky, randomly" 16 "RED")
                            (beside
                             (text "Armored Horn " 16 "yellow")
                             (scale 4/5 super-horn)
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


;**********
; Build Boards
;**********
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

(define (tile-posn-list d)
  (build-posn-list (posn-x WINDOW) (- (posn-y WINDOW) TILE_HEIGHT) '()))


;;builds a list of tiles based on pre-defined stage list
;makes list of t/f the size of x*y
;makes list of positions based on x*y and window size
;uses map to create list of tiles using list of t/f and posns
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
(define HEART (freeze (scale 1/5 happy-walley)))
;;returns scene with info loaded on HUD-AREA
(define (draw-HUD l diff sc)
  ;;create list of component and positions
  (define hud-comp
   (append (for/list ([i (in-range 0 l)])
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
    (append (for/list ([i (in-range 1 (+ l 1))])
               (make-posn (* i 30) 20))
            (list (make-posn 175 20)
                  (make-posn 250 20)
                  (make-posn 250 40)
                  (make-posn 375 20)
                  (make-posn 425 20))))
  (place-images  hud-comp hud-posn HUD-AREA))

;************
; Tile Struct
;************
;;defines struct for tiles
(define-struct/contract tile ([up? boolean?]
                              [position any/c])
  #:transparent)

;;map to determine what image tile to place on the background based on list passed from build-board
(define (place-tiles t)
  (map (lambda (n) (if(tile-up? n) SPOUT_TILE FLOOR_TILE))
       t))  

;***********
; Player Struct
;***********
;;defines and contracts the player object
;;needs to be expanded to include player characteristics
;;this definition type automatically creates:
;  constructor= (make-player state position lives)
;  eq test= player?
;  fields= player-state, player-position, player-lives
(define-struct/contract player ([state (or/c 'swimming
                                             'dead)]
                               [position (or/c #f posn?)]
                               [direction (or/c "right"
                                                "left"
                                                "up"
                                                "down")]
                               [lives (or/c 0 1 2 3)])
  #:transparent)

(define (player-set-pose p position direction)
  (make-player (player-state p) position direction (player-lives p)))
;***********
; STAGES
;***********
;;BEHOLD-Stage function that takes info from the player in order to build the stage
;  for game-play
;;
(define (BEHOLD-Stage diff p sc t)
  (define st 
  (make-stage 'start diff (draw-enemies diff (stage_number diff) t) (swim (player-state p) (player-direction p) t) (draw-HUD (player-lives p) diff sc) (build-board diff)))
  ;;add components to list
  (define stage-comp
    (append (list rainbow (stage-HUD st))
            (place-tiles (stage-board st))
            (for/list ([i (stage-Enemies st)])
                      (cond ((< diff 10) sharkfin)
                            ((< diff 100) laser-shark)
                            ((< diff 1000) sharkfin)))
            (list (scale 1/2 (swim (player-state p) (player-direction p) t)))))
  (define stage-posn
    (append (list (make-posn 50 75) (make-posn (quotient (posn-x WINDOW) 2) 18))
            (tile-posn-list (stage_number diff))
            (for/list ([i (stage-Enemies st)])
                      (shark-p i))
            (list (player-position p))))
  ;;draw stage components
  (if (>= diff 100)
      (create-sharknado t (place-images stage-comp stage-posn BACKGROUND))
      (place-images stage-comp stage-posn BACKGROUND)))
  

;***********
; Stage Struct
;***********
;;defines and contracts the player object
;;needs to be expanded to include player characteristics
;;this definition type automatically creates:
;  constructor= (make-stage state dif Walley HUD Board)
;  eq test= stage?
;  fields= stage-state, stage-difficulty, stage-Walley, stage-HUD, stage-board
(define-struct/contract stage ([state (or/c 'start
                                            'dead
                                            'won
                                            'lost)]
                               [difficulty (and/c natural-number/c (<=/c 1000))]
                               [Enemies (listof shark?)]
                               [Walley any/c]
                               [HUD any/c]
                               [board (listof tile?)])
  #:transparent)



;************
;WORLD-STRUCT
;************
;;defines and contracts the world object
;;needs to be expanded to include: stage-board(board object), player(player object), player-posn(posn)
;;this definition type automatically creates:
;  constructor= (make-world state player)
;  eq test= world?
;  fields= world-state, world-player

(define-struct/contract world ([state (or/c 'splash_screen
                                            'playing
                                            'paused
                                            'won
                                            'lost
                                            'help_screen
                                            'help_screen_2)]
                               [time (and/c natural-number/c (<=/c 100000))]
                               [player (or/c #f player?)]
                               [difficulty (and/c natural-number/c (<=/c 1000))]
                               [score (and/c natural-number/c (>=/c 0))])
  #:transparent)
(define (advance-time s)
  (cond ((eq? (world-state s) 'splash_screen)
         (make-world (world-state s) (if (= 500 (world-time s)) 0 (add1 (world-time s))) (world-player s) (world-difficulty s) (world-score s)))
        (else (make-world (world-state s) (add1 (world-time s)) (world-player s) (world-difficulty s) (world-score s)))))
;**************
;Pad input
;**************
;change handles keyboard input from user
;starts game by calling make-world with new state
; @Jacob-Phillips - can expand to include other menu options
; can change to pad-event instead of key-event
(define (make-world-increase-difficulty s)
        (let* ((current (difficulty-from-stage (world-difficulty s)))
               (updated (cond ((eq? current 1) 10)
                              ((eq? current 10) 100)
                              (else 1))))
              (make-world (world-state s) (world-time s) (world-player s) updated (world-score s))))
(define (make-world-decrease-difficulty s)
        (let* ((current (difficulty-from-stage (world-difficulty s)))
               (updated (cond ((eq? current 1) 100) ; wrap easy => hard
                              ((eq? current 10) 1)
                              (else 10))))
              (make-world (world-state s) (world-time s) (world-player s) updated (world-score s))))

(define (keyboard-handler s ke)
  (cond
   ;;AT PAUSE SCREEN
   ((equal? (world-state s) 'paused)
    (cond ((or (key=? ke "c") (key=? ke "C")) (make-world 'playing (world-time s) (world-player s) (world-difficulty s) (world-score s)))
          ((or (key=? ke "q") (key=? ke "Q")) (make-world 'splash_screen 0 (make-player 'swimming START "left" 3) (stage-reset (world-difficulty s)) 0))
          (else s)))
   ;;Other screens use pad only
   (else s)))

(define (pad-handler s pe)
  (cond
    ;;AT SPLASH SCREEN
    ((equal? (world-state s) 'splash_screen)
     (cond ((pad=? pe " ")      (make-world 'playing     0 (world-player s) (world-difficulty s) (world-score s)))
           ((pad=? pe "rshift") (make-world 'help_screen 0 (world-player s) (world-difficulty s) (world-score s)))
           ((pad=? pe "shift")  (make-world 'help_screen 0 (world-player s) (world-difficulty s) (world-score s)))
           ((pad=? pe "up")   (make-world-decrease-difficulty s)) ; invert up/down to match scroll direction
           ((pad=? pe "down") (make-world-increase-difficulty s)) ; invert up/down to match scroll direction
           (else s)))
    
    ;;AT HELP SCREEN(S)
    ((equal? (world-state s) 'help_screen)
     (cond ((pad=? pe "rshift") (make-world 'splash_screen 0 (world-player s) (world-difficulty s) (world-score s)))
           ((pad=? pe "shift")  (make-world 'splash_screen 0 (world-player s) (world-difficulty s) (world-score s)))
           ((pad=? pe "right")  (make-world 'help_screen_2 0 (world-player s) (world-difficulty s) (world-score s)))
           (else s)))
    ((equal? (world-state s) 'help_screen_2)
     (cond ((pad=? pe "rshift") (make-world 'splash_screen 0 (world-player s) (world-difficulty s) (world-score s)))
           ((pad=? pe "shift")  (make-world 'splash_screen 0 (world-player s) (world-difficulty s) (world-score s)))
           ((pad=? pe "left")   (make-world 'help_screen   0 (world-player s) (world-difficulty s) (world-score s)))
           (else s)))
    
    ;;AT PLAYING SCREEN
    ((equal? (world-state s) 'playing)
     (cond 
           ((pad=? pe "rshift") (make-world 'paused 0 (world-player s) (world-difficulty s) (world-score s)))
           ((pad=? pe "shift")  (make-world 'paused 0 (world-player s) (world-difficulty s) (world-score s)))
           ((pad=? pe "right")  (make-world 'playing (world-time s) (move_walley (world-player s) "right") (world-difficulty s) (world-score s)))
           ((pad=? pe "left")   (make-world 'playing (world-time s) (move_walley (world-player s) "left")  (world-difficulty s) (world-score s)))
           ((pad=? pe "up")     (make-world 'playing (world-time s) (move_walley (world-player s) "up")    (world-difficulty s) (world-score s)))
           ((pad=? pe "down")   (make-world 'playing (world-time s) (move_walley (world-player s) "down")  (world-difficulty s) (world-score s)))
           ((pad=? pe "d")      (make-world 'playing (world-time s) (move_walley (world-player s) "right") (world-difficulty s) (world-score s)))
           ((pad=? pe "a")      (make-world 'playing (world-time s) (move_walley (world-player s) "left")  (world-difficulty s) (world-score s)))
           
           ;;test for win/stage prog- w for win-test, s for die test
           ((and (pad=? pe "w") (= (world-difficulty s) 900)
                           (make-world 'won 0 (world-player s) 1 (world-score s))))
           ((pad=? pe "w") (make-world 'playing 0 (player-set-pose (world-player s) START "left") (next-stage (world-difficulty s)) (+ (world-score s) 100)))
           ;;put player back at Start, decrease lives
           ((and (pad=? pe "s") (= (player-lives (world-player s)) 0))
                           (make-world 'lost    0 (world-player s) (world-difficulty s) (world-score s)))
           ((pad=? pe "s") (make-world 'playing 0
                                                  (make-player (player-state (world-player s)) START "left" (- (player-lives (world-player s)) 1))
                                                                   (world-difficulty s) (world-score s)))
           (else s)))
    
    ;;AT WON SCREEN
    ((equal? (world-state s) 'won)
     (cond ((pad=? pe " ")      (make-world 'playing 0 (make-player 'swimming START "left" 3) (stage-reset (world-difficulty s)) 0))
           ((pad=? pe "rshift") (make-world 'help_screen 0 (world-player s) (world-difficulty s) (world-score s)))
           ((pad=? pe "shift")  (make-world 'help_screen 0 (world-player s) (world-difficulty s) (world-score s)))
           ((pad=? pe "up")     (make-world-decrease-difficulty s)) ; invert up/down to match scroll direction
           ((pad=? pe "down")   (make-world-increase-difficulty s)) ; invert up/down to match scroll direction
           (else s)))
    ;;AT LOST SCREEN
    ((equal? (world-state s) 'lost)
     (cond ((pad=? pe " ")      (make-world 'playing 0 (make-player 'swimming START "left" 3) (stage-reset (world-difficulty s)) 0))
           ((pad=? pe "rshift") (make-world 'help_screen 0 (world-player s) (world-difficulty s) (world-score s)))
           ((pad=? pe "shift")  (make-world 'help_screen 0 (world-player s) (world-difficulty s) (world-score s)))
           ((pad=? pe "up")     (make-world-decrease-difficulty s)) ; invert up/down to match scroll direction
           ((pad=? pe "down")   (make-world-increase-difficulty s)) ; invert up/down to match scroll direction
           (else s)))
     
   (else s) ; unsupported world-state
   ))

 ;;make worlds
;;make-initial-world(create the splash_screen)
(define (make-water-world)
  (make-world 'splash_screen 0 (make-player 'swimming START "left" 3) 1 0))

;;render-world
;;draws the world based on the state of the world
;;must take a world-state as an arg because it is called in 'big-bang'
; result of condition needs to be expanded to include functions
(define (render-world s)
  (cond
    ((equal? (world-state s) 'splash_screen) (render-splashscreen s))
    ((equal? (world-state s) 'help_screen) (render-helpscreen s))
    ((equal? (world-state s) 'help_screen_2) (render-helpscreen s))
    ((equal? (world-state s) 'playing) (BEHOLD-Stage (world-difficulty s) (world-player s) (world-score s) (world-time s)))
    ((equal? (world-state s) 'paused) (render-pausedscreen s))
    ((equal? (world-state s) 'lost) (render-splashscreen s))
    (else (render-splashscreen s))))


;;***********
;;main driver
;;***********
;;defines main funtion(no args)
; calls big-bang with initial state of (make-water-world) which initially creates the splash_screen
; each clause in big-bang must take the state of the world as an arg, and returns a new state
  (define (main)
  (big-bang (make-water-world)
            [on-tick advance-time]
    [to-draw render-world]
    [name "Pelagic-Kong"]
    [on-key keyboard-handler]
    [on-pad pad-handler]
    [close-on-stop #t]))

(main)