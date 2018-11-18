;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |4.0|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)



;; One unit will be 25 pixels long, one square unit will be 25 by 25
(define UNIT 25)
(define HALF (/ UNIT 2))

;; All of the modes will be solid
(define MODE 'solid)

;; Colors:
;;         Grey
(define GREY (make-color 120 120 120 255))
;;         Burgandy
(define BURGANDY (make-color 144 0 32 255))

;; One square block of a piece will also be UNIT by UNIT, but will be grey with
;; a burgandy outline

(define PIECE
  (overlay
   ;; inner grey square
   (square (- UNIT 5) MODE
           GREY)
   ;; burgandy outline
   (square UNIT MODE
           BURGANDY)))

;; The function will be run on an empty scene of width 10*UNIT by height
;; 20*UNIT pixels
(define WIDTH (* 10 UNIT))
(define HEIGHT (* 20 UNIT))
(define MT (empty-scene WIDTH HEIGHT (make-color 200 200 200 255)))

;; RATE at which the system clock will tick
(define RATE .7)


(define-struct world (piece edge))

(define-struct piece (lisposn rot))

(define (I)
  (let
      [(decision (random 2))
       (center0 (+ (/ UNIT 2) (* UNIT (random 7))))
       (center1 (+ (/ UNIT 2) (* UNIT (random 10))))]
    (cond
      [(= 0 decision)
       (make-piece
        (build-list
         4
         (lambda (x)
           (make-posn
            (+ center0 (* UNIT x))
            (- HALF))))
        false)]
      [(= 1 decision)
       (make-piece
        (build-list
         4
         (lambda (x)
           (make-posn
            center1
            (- (- HALF) (* UNIT x)))))
        true)])))

(define (J)
  (let
      [(center (+ HALF (* UNIT (random 8))))]
    (make-piece
     (cons
      (make-posn (+ UNIT center) (- HALF))
      (build-list
       3
       (lambda (x)
         (make-posn
          center
          (- (- HALF) (* UNIT x))))))
     false)))

(define (L)
  (let
      [(center (+ HALF (* UNIT (add1 (random 8)))))]
    (make-piece
     (cons
      (make-posn center (- HALF))
      (build-list
       3
       (lambda (x)
         (make-posn
          (+ UNIT center)
          (- (- HALF) (* UNIT x))))))
     false)))

(define (O)
  (let
      [(center (+ HALF (* UNIT (random 9))))]
    (make-piece
     (list
      (make-posn center (- HALF))
      (make-posn center (+ (- HALF) UNIT))
      (make-posn (+ center UNIT) (- HALF))
      (make-posn (+ center UNIT) (+ (- HALF) UNIT)))
     false)))

(define (S)
  (let
      [(center (+ HALF (* UNIT (random 8))))]
    (make-piece
     (list
      (make-posn center (- HALF))
      (make-posn (+ center UNIT) (- HALF))
      (make-posn (+ center UNIT) (+ UNIT (- HALF)))
      (make-posn (+ center (* 2 UNIT)) (+ UNIT (- HALF))))
     false)))

(define (Z)
  (let
      [(center (+ HALF (* UNIT (random 8))))]
    (make-piece
     (list
      (make-posn center (- (- HALF) UNIT))
      (make-posn (+ center UNIT) (- (- HALF) UNIT))
      (make-posn (+ center UNIT) (- HALF))
      (make-posn (+ center (* 2 UNIT)) (- HALF)))
     false)))

(define (T)
  (let
      [(center (+ HALF (* UNIT (random 8))))]
    (make-piece
     (list
      (make-posn center (- HALF))
      (make-posn (+ center UNIT) (- HALF))
      (make-posn (+ center UNIT) (- (- HALF) UNIT))
      (make-posn (+ center (* 2 UNIT)) (- HALF)))
     false)))

(define bottom
  (build-list
   10
   (lambda (x)
     (make-posn
      (+ HALF (* UNIT x))
      (+ HEIGHT HALF)))))

(define (selector-helper n)
  (cond
    ;; n=0, return I
    [(= n 0) (I)]
    ;; n=1, return J
    [(= n 1) (J)]
    ;; n=2, return L
    [(= n 2) (L)]
    ;; n=3, return O
    [(= n 3) (O)]
    ;; n=4, return S
    [(= n 4) (S)]
    ;; n=5, return Z
    [(= n 5) (Z)]
    ;; n=6, return T
    [(= n 6) (T)]))


;; Contract: (selector) -> image (one of the pieces)
;; Input:    none
;; Purpose:  Randomly selects one of the seven pieces.

;; Pre-function tests are not possible due to random output

;; Function definition:

(define (selector)
  ;; call selector-helper on a random number between 0 and 6
  (selector-helper
   (random 7)))

(define start
  (make-world
   (selector) bottom))

(define (main)
  (big-bang
   start
   ;; Causes a call-back to tock every RATE seconds
   (on-tick tock RATE)
   ;; Call change when a key is pressed
   (on-key key-handler)
   ;; Causes a call-back to render every time system values change
   (to-draw render)
   (stop-when stop? final-render)))

(define (move-edge edge)
  (map
   (lambda (posn)
     (make-posn
      (posn-x posn)
      (+ (posn-y posn) UNIT)))
   edge))

(define (move world)
  (make-world
   (make-piece
    (map
     (lambda (posn)
       (make-posn
        (posn-x posn)
        (+ (posn-y posn) UNIT)))
     (piece-lisposn (world-piece world)))
    (piece-rot (world-piece world)))
   (world-edge world)))

(define (edge? world)
  (ormap
   ;; function checking if a posn is equal to any of the entries in edge
   (lambda (posn1)
     (ormap
      ;; function 
      (lambda (posn2)
        (equal? posn1 posn2))
      (world-edge world)))
   (piece-lisposn (world-piece world))))

(define (will-be-edge? world)
  (edge?
   (move world)))

(define (full-row? rownum edge)
  (<= 10 
      (length
       (filter
        (lambda (posn)
          (= (posn-y posn)
             (+ HALF (* UNIT rownum))))
        edge))))

(define (full-row-posy world acc)
  (cond
    [(full-row? acc (world-edge world)) acc]
    [else
     (full-row-posy world (add1 acc))]))  

(define (full-row-somewhere? world)
  (letrec
      [(full-row-acc
        (lambda (acc)
          (cond
            [(= acc 0)
             (full-row? acc (world-edge world))]
            [else
             (or
              (full-row? acc (world-edge world))
              (full-row-acc (sub1 acc)))])))]
    (full-row-acc 19)))

(define (row-remove world)
  (let* [(row-num (full-row-posy world 0))
         (removed-edge (filter
                        (lambda (posn)
                          (not
                           (= row-num
                              (posn-y posn))))
                        (world-edge world)))]
    (make-world
     (world-piece world)
     (append
      (filter
       (lambda (posn)
         (> (posn-y posn)
            (+ HALF (* UNIT row-num))))
       (world-edge world))
      (move-edge
       (filter
        (lambda (posn)
          (< (posn-y posn)
             (+ HALF (* UNIT row-num))))
        (world-edge world)))))))

(define (tock world)
  (cond
    [(full-row-somewhere? world)
     (row-remove world)]
    [(will-be-edge? world)
     (make-world
      (selector)
      (append (piece-lisposn (world-piece world)) (world-edge world)))]
    [else
     (move world)]))

(define (render world)
  (let*
      [(all-pieces (append (piece-lisposn (world-piece world))
                           (world-edge world)))
       (number (length all-pieces))]
    (place-images
     (make-list number PIECE)
     all-pieces
     MT)))

(define (stop? world)
  (ormap
   (lambda (posn)
     (> 0 (posn-y posn)))
   (world-edge (tock world))))

(define (final-render world)
  (overlay
   (text "You're bad and you should feel bad -.- suks 2 suk" 12 'black)
   (render world)))

(define (move-left world)
  (make-world
   (make-piece
    (map
     (lambda (posn)
       (make-posn
        (- (posn-x posn)
           UNIT)
        (posn-y posn)))
     (piece-lisposn (world-piece world)))
    (piece-rot (world-piece world)))
   (world-edge world)))

(define (move-right world)
  (make-world
   (make-piece
    (map
     (lambda (posn)
       (make-posn
        (+ (posn-x posn)
           UNIT)
        (posn-y posn)))
     (piece-lisposn (world-piece world)))
    (piece-rot (world-piece world)))
   (world-edge world)))

(define (off-side? world)
  (ormap
   (lambda (posn)
     (or (< WIDTH (posn-x posn))
         (> 0 (posn-x posn))))
   (piece-lisposn (world-piece world))))

(define (call-unless f world)
  (cond
    [(or (off-side? (f world))
         (edge? (f world)))
     world]
    [else
     (f world)]))

(define (move-down world)
  (make-world
   (make-piece
    (move-edge (piece-lisposn (world-piece world)))
    (piece-rot (world-piece world)))
   (world-edge world)))


(define (rotate-piece world)
  (local
    [(define center (list-ref (piece-lisposn (world-piece world)) 0))
     (define rot? (piece-rot (world-piece world)))
     (define (switch-x-y pos) ;; rotates the pieces
       (if rot?
           (make-posn
            (+ (posn-x center) (- (posn-y center) (posn-y pos)))
            (+ (posn-y center) (- (posn-x center) (posn-x pos)))
            )
           (make-posn
            (- (posn-x center) (- (posn-y center) (posn-y pos)))
            (- (posn-y center) (- (posn-x center) (posn-x pos)))
            )))]
(make-world ;; makes a new world state with the rotated piece
 (make-piece
  (map switch-x-y
       (piece-lisposn (world-piece world)))
  (not (piece-rot (world-piece world))))
 (world-edge world))))


(define (key-handler world kee)
  (cond
    [(string=? kee "left")
     (call-unless move-left world)]
    [(string=? kee "right")
     (call-unless move-right world)]
    [(string=? kee "down")
     (call-unless move-down world)]
    [(string=? kee "up")
     (call-unless rotate-piece world)]
    [else world]))




(main)




