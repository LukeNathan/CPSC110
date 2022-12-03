;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-03-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

(@assignment labs/lab-03)

;; If you are:
;;   - A 110 or 107 student replace the first set of '???'s with your cwl.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   - A UBC Extended Learning student, replace the first set of ??? with
;;     the your email address as confirmed in the email you received from
;;     extended learning.  The handin password is also in that email.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   
(@cwl lnatha01 ???)

; Balloon popping

(@htdw Balloon)
;; CONSTANTS ==========================

(define WIDTH 500)
(define HEIGHT 500)
(define MTS (empty-scene WIDTH HEIGHT))

(define BALLOON-COLOR "red")
(define POP-IMAGE
  (overlay (text "POP!" 80 "black")
           (radial-star 30 (/ WIDTH 10) (/ WIDTH 2) "solid" "yellow")))

(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))

(define SPEED 2)

(define MAX-SIZE (/ WIDTH 2))





;; DATA DEFINITIONS ============================
 
(@problem 1)

(@htdd Balloon)
;; Balloon is one of:
;; - Number
;; - false
;; interp. Number is balloon radius, falso means no balloon (popped)

(define B1 5)     ;Number, 5 = balloon radius
(define B2 false) ;balloon is popped

(@dd-template-rules one-of
                    atomic-non-distinct ;Number
                    atomic-distinct)    ;false

(define (fn-for-balloon b)
  (cond [(number? b) (... b)]
        [else (...)]))

;; FUNCTIONS ====================================
(@problem 2)
(@htdf main)
(@signature Balloon -> Balloon)  
;; starts the world program with !!!
; no examples for main function

(@template-origin htdw-main)
(define (main b)
  (big-bang b                ; Balloon
            (on-tick tick)   ; Balloon -> Balloon
            (to-draw render) ; Balloon -> Image
;           (stop-when ...)  ; WS -> Boolean
;           (on-mouse ...)   ; WS Integer Integer MouseEvent -> WS
;           (on-key ...)     ; WS KeyEvent -> WS
            ))

(@problem 3)
(@htdf tick)
(@signature Balloon -> Balloon) 
;; produce the next ...
;; !!!

(check-expect (tick 50) (+ 50 SPEED))
(check-expect (tick (- MAX-SIZE 1 SPEED)) (- MAX-SIZE 1))
(check-expect (tick (- MAX-SIZE SPEED)) MAX-SIZE)
(check-expect (tick (- MAX-SIZE (- SPEED 1))) false)
(check-expect (tick MAX-SIZE) false)
(check-expect (tick 0) SPEED)
(check-expect (tick false) false)
;(define (tick b) ...)

(@template-origin Balloon)

(@template
 (define (fn-for-balloon b)
  (cond [(number? b) (... b)]
        [else (...)])))

(define (tick b)
  (cond [(number? b) (if (<= (+ b SPEED) MAX-SIZE)
                         (+ b SPEED)
                         false)]
        [else false]))


(@problem 4)
(@htdf render)
(@signature Balloon -> Image) 
;; render ...
;; !!!

(check-expect (render 10)
              (place-image (circle 10 "solid" BALLOON-COLOR)
                           CTR-X CTR-Y MTS))

(check-expect (render (- MAX-SIZE 1))
              (place-image (circle (- MAX-SIZE 1)
                                   "solid" BALLOON-COLOR) CTR-X CTR-Y MTS))

(check-expect (render MAX-SIZE)
              (place-image (circle MAX-SIZE
                                   "solid" BALLOON-COLOR) CTR-X CTR-Y MTS))

(check-expect (render false)
              (place-image POP-IMAGE CTR-X CTR-Y MTS))

(check-expect (render 0)
              (place-image (circle 0 "solid" BALLOON-COLOR)
                           CTR-X CTR-Y MTS))

;(define (render ws) ...)

(@template-origin Balloon)

(@template
 (define (fn-for-balloon b)
  (cond [(number? b) (... b)]
        [else (...)])))

 (define (render b)
  (cond [(number? b) (place-image (circle b
                      "solid" BALLOON-COLOR) CTR-X CTR-Y MTS)]
        [else (place-image POP-IMAGE CTR-X CTR-Y MTS)]))

