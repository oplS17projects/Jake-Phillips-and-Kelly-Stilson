# Pelagic-Kong

## Jacob Phillips
### April 30, 2017

# Overview
This is a playable game loosely based off of the original Donkey Kong Game.

There are 30 total levels(3 difficulties 10 levels each.)

The user uses the arrow keys to avoid sharks and climb the spouts up to the rainbow to win.

The spouts are the ladders, the sharks are the barrels and getting to the rainbow is saving the princess.

<img src="images/Release-Demo_gameplay.PNG" alt="Splash Screen" width="400"/> <img src="images/donkey-kong.jpg" alt="Splash Screen" width="400"/> 

**Authorship note:** All of the code described here was written by myself unless stated otherwise.

# Libraries Used
The code uses three libraries:

```
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
```

* The ```2htdp/image``` library provides the ability to draw all graphics used in the game.
* The ```2htdp/universe``` library is used to 
* The ```lang/posn``` library is used to 

## 1. Applying Functional Approaches for handling user input

The world structure recieves a lambda that determines how the user input will be processed. 
```
(define-struct/contract world (... [inputHandler (-> any/c (or/c pad-event? key-event?) any/c)])
```
The world structure's inputHandler is envoked by the pad-handler call back, which is invoked by big bang's on-pad event. 
```
 (define (main)
  (big-bang (make-water-world)
  ...
    [on-pad pad-handler]
   ...)
```
```
(define (pad-handler s pe) ((world-inputHandler s) s pe))
```
An example of a world-inputHandler is the playing-input-handler.
```
(define playing-input-handler
   (lambda (wrld input)
     (cond ...
           ((pad=? input "rshift") (make-world 'paused 0 player difficulty_level score stage paused-input-handler))
           ...
           (else wrld)))))
```
The world-inputHandler is assigned in the world constructor. 
```
(define gameover-input-handler
   (lambda (wrld input)
     ...
     (cond ((pad=? input " ")      (make-world 'playing 0 (make-player 'swimming START "left" 3) (stage-reset difficulty_level) 0 stage playing-input-handler))
          ...)
```

## 2. Move Player using Object Orientation

I designed functions around manipulating objects, such as the player object. The API naming conventions refer to object
types and actions on those object types. 
```
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
```
## 3. World Evolution using Recursion
The world is initially created through a call to make-world. The last argument to make-world is a lamda function that itself calls make-world. The following code is executed when first entering the game screen.
```
(make-world 'start 0 (make-player 'swimming START "left" 3) (world-difficulty wrld) 0
                          (make-stage 'start 1
                                      (draw-enemies (world-difficulty wrld) 1 1)
                                      happy-walley
                                      draw-HUD 3 (world-difficulty wrld) 0)
                                      (build-board (world-difficulty wrld))
                                      (make-super-powers #f #f #f))
                          playing-input-handler))
```
The next code snippet is executed by the playing-input-handler lambda function.
```
((pad=? input "up")     (make-world 'playing time (move-player wrld "up")    difficulty_level score stage (world-inputHandler wrld)))
```
## 4. Touching Spout Tile? Function using Data Abstraction
Similar to the way object orientation was applied, data abstraction was used to insulate application code. In this case
the API abstracts the data structure used to store the tiles. For example, touching-spout-tile? does not require the caller 
to have any knowledge of whats in memory. Designing the API this way mitigates the cost of changing the internal data structures.
For example, the tiles structures are currently stored in a single dimensional array and changing to a two dimensional array
would not require modifications to any code calling touching-spout-tile?.

```
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
```
