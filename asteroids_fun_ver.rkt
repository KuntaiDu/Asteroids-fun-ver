#lang racket

#|

Based on @Eric Clack's asteroids5

Asteroids - (go) to run.

Left / right to rotate
Up / down to speed up, slow down
Space to fire.

DONE:
- Multiple key presses, e.g. moving and shooting at the
  same time.
- Some motivaton for the user to do more than just fire
  in circles endlessely - maybe limitted bullets?
- Live system and tool system had been installed.
- Fancy images plugged!

TODO:
- Difficult

|#



(require 2htdp/universe 2htdp/image 2htdp/planetcute)
(require "util.rkt")

;; Debug
(require unstable/debug)
(require racket/trace)

(struct world (asteroids ship bullets tools score life) #:transparent #:mutable)
(struct pos (x y) #:transparent #:mutable)
(struct ship (pos direction speed) #:transparent #:mutable)
(struct asteroid (pos direction speed size) #:transparent #:mutable)
(struct bullet (pos direction speed) #:transparent #:mutable)
(struct tool (pos direction speed id) #:transparent)

(define BIG-ASTEROID 60)
(define NUM-ASTEROIDS 15)

(define SHIP-SIZE 30)

(define BULLET-SPEED 5)
(define MAX-BULLETS 15)

(define TOOL-SIZE 30)

(define MAX-TOOLS 8)


(define TICK-RATE 1/30)
(define WIDTH 800)
(define HEIGHT 600)

(define KEY-STATE (make-hash))

(define START? #t)
(define GAMEOVER? #f)
(define NEWGAME? #f)

;;-----------------general process---------------------

(define (move-pos a-pos a-direction a-speed)
  (define r (degrees->radians a-direction))
  (pos (+ (pos-x a-pos) (* a-speed (cos r)))
       (+ (pos-y a-pos) (* a-speed (sin r)))))

(define (wrap-pos a-pos a-size)
  (define x (pos-x a-pos))
  (define y (pos-y a-pos))
  (pos (cond
         [(> x (+ WIDTH a-size)) (- 0 a-size)]
         [(< x (- 0 a-size)) (+ WIDTH a-size)]
         [else x])
       (cond
         [(> y (+ HEIGHT a-size)) (- 0 a-size)]
         [(< y (- 0 a-size)) (+ HEIGHT a-size)]
         [else y])))

(define (inside-circle circle-pos radius a-pos)
  (define distance
    (sqrt (+ (expt (- (pos-x a-pos) (pos-x circle-pos)) 2)
             (expt (- (pos-y a-pos) (pos-y circle-pos)) 2))))
  (<= distance radius))



;;-------------------asteroid--------------------

(define (new-asteroid)
  (asteroid (pos (random WIDTH) (random HEIGHT))
            (random 360) (+ 1 (random 2)) BIG-ASTEROID))

(define (move-asteroid a)
  (asteroid (wrap-pos
             (move-pos (asteroid-pos a) (asteroid-direction a) (asteroid-speed a))
             (asteroid-size a))
            (asteroid-direction a)
            (asteroid-speed a)
            (asteroid-size a)))

(define (asteroids-diff prev-asteroids next-asteroids)
  ;; +1 point each time the number of asteroids decreases
  ;; regardless of size
  (define diff (- (length prev-asteroids)
                  (length next-asteroids)))
  (if (> diff 0) diff 0))



;;--------------------bullet-------------------

(define (new-bullet a-ship)
  (bullet (ship-pos a-ship)
          (ship-direction a-ship)
          (+ (ship-speed a-ship) BULLET-SPEED)))

(define (move-bullet b)
  (bullet (move-pos (bullet-pos b) (bullet-direction b) (bullet-speed b))
          (bullet-direction b)
          (bullet-speed b)))

(define (bullet-in-range a-bullet)
  (define x (pos-x (bullet-pos a-bullet)))
  (define y (pos-y (bullet-pos a-bullet)))
  (and (> x 0) (< x WIDTH) (> y 0) (< y HEIGHT)))



;;---------------------ship-----------------
(define (move-ship a-ship)
  (ship (wrap-pos
         (move-pos (ship-pos a-ship) (ship-direction a-ship) (ship-speed a-ship))
         SHIP-SIZE)
        (ship-direction a-ship)
        (ship-speed a-ship)))



;;---------------------tool--------------------

(define (new-tool)
  (tool (pos (random WIDTH) (random HEIGHT))
        (random 360)
        (+ 1 (random 2))
        (random-range 1 3)))

(define (move-tool t)
  (tool (wrap-pos
         (move-pos (tool-pos t) (tool-direction t) (tool-speed t))
         TOOL-SIZE)
        (tool-direction t)
        (tool-speed t)
        (tool-id t)))



;;------------------interaction---------------

(define (hit-asteroids asteroids bullets)
  ;; If any asteroids have been hit, split them in half.
  ;; Asteroids that are too small are deleted.
  
  ;; A list like this (a a a a a) will result in a list
  ;; like this (a a (a a) a a) on hit, we use flatten
  ;; to return the right thing.
 
  (define (hit-asteroid? a bullets)
    ;; Has this asteroid been hit by any of the bullets?
    (cond
      [(empty? bullets) #f]
      [(inside-circle (asteroid-pos a) (asteroid-size a)
                      (bullet-pos (car bullets))) #t]
      [else
       (hit-asteroid? a (cdr bullets))]))

  (define (split-asteroid a)
    (list (asteroid (asteroid-pos a) (- (asteroid-direction a) 90)
                    (asteroid-speed a) (/ (asteroid-size a) 2))
          (asteroid (asteroid-pos a) (+ (asteroid-direction a) 90)
                    (asteroid-speed a) (/ (asteroid-size a) 2))))

  (define (bullets-hit-asteroid a)
    (if (hit-asteroid? a bullets)
        (split-asteroid a)
        a))

  (define (big-enough a)
    (> (asteroid-size a) 5))
  
  (filter big-enough (flatten (map bullets-hit-asteroid asteroids))))


(define (live-bullets asteroids bullets)
  ;; Like hit-asteroids, but returns only bullets that
  ;; have not hit an asteroid

  (define (bullet-hit? b asteroids)
    (cond
      [(empty? asteroids) #f]
      [(inside-circle (asteroid-pos (car asteroids))
                      (asteroid-size (car asteroids))
                      (bullet-pos b)) #t]
      [else (bullet-hit? b (cdr asteroids))]))
  
  (define (bullet-hit-no-asteroids b)
    (not (bullet-hit? b asteroids)))

  (filter bullet-hit-no-asteroids bullets))


(define (use-tools w a-ship tools asteroids)
  (define (grab-tool? s t)
    (inside-circle (ship-pos s) (+ SHIP-SIZE TOOL-SIZE)
                   (tool-pos t)))

  (define (effect s t asteroids)
    (cond
      [(= (tool-id t) 1) (set-world-life! w (+ 1 (world-life w)))]
      [(= (tool-id t) 2) (set! MAX-BULLETS (* MAX-BULLETS 2))]
      [(= (tool-id t) 3) (map (λ(a) (set-asteroid-speed! a (/ (asteroid-speed a) 2))) asteroids)]
      (else (error "invalid tools"))))

  (define (tool-remain t)
    (not (grab-tool? a-ship t)))
  
  (begin
    (map (λ(t) (when(grab-tool? a-ship t) (effect a-ship t asteroids))) tools)
    (filter tool-remain tools)))
  
  
;;-----------------world----------------

(define (next-world w)
  (move-world (direct-ship w)))

(define (move-world w)
  (define next-asteroids (hit-asteroids (world-asteroids w) (world-bullets w)))
  (define next-bullets (live-bullets (world-asteroids w) (world-bullets w)))
  (define next-tools (use-tools w (world-ship w) (world-tools w) (world-asteroids w)))
  (define add-score (asteroids-diff (world-asteroids w) next-asteroids))
  (define a-ship (ship (pos (/ WIDTH 2) (/ HEIGHT 2)) -90 0))
  (define (check-hit)
    (set-world-life! w  (- (world-life w) 1))
    (world-life w))

  (if (ship-crashed? w)
      (world (map move-asteroid next-asteroids)
         a-ship
         (filter bullet-in-range (map move-bullet next-bullets))
         (map move-tool next-tools)
         (+ add-score (world-score w))
         (check-hit))
      (world (map move-asteroid next-asteroids)
         (move-ship (world-ship w))
         (filter bullet-in-range (map move-bullet next-bullets))
         (map move-tool next-tools)
         (+ add-score (world-score w))
         (world-life w))))

(define (next-or-over w)
  (if NEWGAME?
      (begin
        (set! GAMEOVER? #f)
        (set! NEWGAME? #f)
        (new-world))
      (if (or (life-is-zero? w) (empty? (world-asteroids w)))
          (begin
            (set! GAMEOVER? #t)
            w)
          (next-world w))))
(define (life-is-zero? w)
  (equal? (world-life w) 0))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Rendering

(define (img+scene pos img scene)
  (place-image img (pos-x pos) (pos-y pos) scene))

(define (ship-img a-direction)
   ;(overlay
   ;(rotate (- 270 a-direction)
         ; (overlay/offset (triangle SHIP-SIZE "solid" "white") 0 8
                ;          (triangle SHIP-SIZE "solid" "white")))
   ;(circle (/ SHIP-SIZE 2) "outline" "red")))
  (let ((fancy-img (overlay/xy character-cat-girl
                               0 10
                               selector)))
   (rotate (- 270 a-direction)
           (overlay
            (scale (* (/ SHIP-SIZE (image-height fancy-img)) 3) fancy-img)
            (circle (/ SHIP-SIZE 2) "outline" "white")))))
                  
            

(define (ship+scene a-ship scene)
  (img+scene (ship-pos a-ship)
             (ship-img (ship-direction a-ship))
             scene))

(define (tool-img t)
  (let ((resize-factor
    (/ TOOL-SIZE BIG-ASTEROID)))
    (define name
      (cond
        [(= (tool-id t) 1) "LIFE"]
        [(= (tool-id t) 2) "BULLET"]
        [(= (tool-id t) 3) "SLOW"]
        (else (error "invalid tools"))))
    (define img
      (cond
        [(= (tool-id t) 1) (scale resize-factor heart)]
        [(= (tool-id t) 2) (scale resize-factor gem-green)]
        [(= (tool-id t) 3) (scale resize-factor gem-blue)]
        (else (error "invalid tools"))))

    (overlay
     (text/font name 16 "white"
             "Gill Sans" 'swiss 'normal 'bold #f)
     ;(circle TOOL-SIZE "solid" "red")
     img)))

(define (tools+scene tools scene)
  (foldl (λ (t scene)
           (img+scene (tool-pos t)
                      (tool-img t)
                      scene))
           scene tools))

(define (asteroids+scene asteroids scene)
  (foldl (λ (a scene)
           (img+scene (asteroid-pos a)
                      ;(circle (asteroid-size a) "solid" "gray")
                      (rotate (asteroid-direction a)
                              (scale (/ (asteroid-size a) BIG-ASTEROID) rock))
                      scene))
         scene asteroids))

(define (bullets+scene bullets scene)
  (foldl (λ (b scene)
           (img+scene (bullet-pos b)
                      ;(circle 2 "solid" "yellow")
                      (scale (/ 20 (image-height yellow-star)) yellow-star)
                      scene))
         scene bullets))

(define (score+scene score life scene)
  (place-image (text (string-append "Score: "
                                    (number->string score))
                     24 "white") 70 80
                                 (place-image (text (string-append "Lives: "
                                                                   (number->string life))
                                                    24 "white")70 50 scene)))

(define (background)
  (define (make-grey n)
    (make-color n n n))
  (place-image
   (underlay (circle (/ HEIGHT 2) "solid" (make-grey 20))
             (circle (/ HEIGHT 3) "solid" (make-grey 10)))
   (/ WIDTH 2) (/ HEIGHT 2)
   (empty-scene WIDTH HEIGHT "black")))

(define (welcome+scene scene)
  (place-image (text "Welcome! Space to shoot, ↑↓←→to move. Press space to start." 24 "white")
               (/ WIDTH 2) (/ HEIGHT 2) scene))

(define (gameover+scene w scene)
  (place-image (text
                (string-append "You died. Press space to restart. Score: "
                               (number->string (world-score w)))
                 24 "red")
               (/ WIDTH 2) (/ HEIGHT 2) scene))

(define (victory+scene w scene)
  (place-image (text (string-append "Victory! Press space to enjoy a new journey! Score: "
                                    (number->string (+ (world-score w)
                                                       (world-life w))))
                     24 "yellow")
               (/ WIDTH 2) (/ HEIGHT 2) scene))

(define (render-world w)
  (score+scene (world-score w) (world-life w)
               (tools+scene (world-tools w)
                            (ship+scene (world-ship w)
                                        (asteroids+scene (world-asteroids w)
                                                         (bullets+scene (world-bullets w)
                                                                        (background)))))))
(define (wrap-render-world w)
  (cond [START? (welcome+scene (background))]
        [(and GAMEOVER? (empty? (world-asteroids w))) (victory+scene w (background))]
        [GAMEOVER? (gameover+scene w (background))]
        [else (render-world w)]))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (key-down w a-key)
  (hash-set! KEY-STATE a-key #t)
  (when (and (key-pressed? " ") GAMEOVER?)
    (set! NEWGAME? #t))
  (when (key-pressed? " ")
    (set! START? #f))
  w)

(define (key-up w a-key)
  (hash-remove! KEY-STATE a-key)
  w)

(define (key-pressed? a-key)
  (hash-ref KEY-STATE a-key #f))

(define (direct-ship w)
  (define a-ship (world-ship w))
  (define a-direction
    (+ (ship-direction a-ship)
    (cond
      [(key-pressed? "left") -5]
      [(key-pressed? "right") 5]
      [else 0])))
  (define a-speed
    (+ (ship-speed a-ship)
       (cond
         [(key-pressed? "up") 1]
         [(key-pressed? "down") -1]
         [else 0])))
  (define bullets
    (cond
      [(and (key-pressed? " ")
            (< (length (world-bullets w)) MAX-BULLETS)) 
       (cons (new-bullet a-ship) (world-bullets w))]
      [else (world-bullets w)]))
  
  (world (world-asteroids w)
         (ship (ship-pos a-ship) a-direction a-speed)
         bullets
         (world-tools w)
         (world-score w)
         (world-life w)))

(define (ship-crashed? w)
  (define a-ship (world-ship w))
  (define (ship-hit-asteroids? asteroids)
    (cond
      [(empty? asteroids) #f]
      [(inside-circle (asteroid-pos (car asteroids))
                      (+ (asteroid-size (car asteroids))
                         (/ SHIP-SIZE 2))
                      (ship-pos a-ship)) #t]
      [else (ship-hit-asteroids? (cdr asteroids))]))

  (ship-hit-asteroids? (world-asteroids w)))

(define (new-world)
  ;; Produce a world in which the ship has not just crashed
  (set! MAX-BULLETS 15)
  (define asteroids (times-repeat NUM-ASTEROIDS (new-asteroid)))
  (define a-ship (ship (pos (/ WIDTH 2) (/ HEIGHT 2)) -90 0))
  (define tools (times-repeat (random-range (/ MAX-TOOLS 2) MAX-TOOLS) (new-tool)))
  
  (define a-world
    (world asteroids a-ship '() tools 0 3))

  (if (ship-crashed? a-world)
      (new-world)
      a-world))
  
(define (go)
  (hash-clear! KEY-STATE)
  (big-bang (new-world)
            (on-tick next-or-over TICK-RATE)
            (on-key key-down)
            (on-release key-up)
            (to-draw wrap-render-world)))

(go)