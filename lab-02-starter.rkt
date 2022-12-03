;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-02-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(@assignment labs/lab-02)

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

;; HtDF Lab, Problem 1

;; PROBLEM:
;;
;; Design a function called square? that consumes an image and determines 
;; whether the image's height is the same as the image's width.
(@problem 1)
(@htdf square?) ;!!!UNCOMMENT this line when you start on this function
(@signature Image -> Boolean)
;; produces true if an image's height is equal to its width

(check-expect (square? (rectangle 50 50 "solid" "red")) true)
(check-expect (square? (rectangle 40 50 "solid" "blue")) false)
(check-expect (square? (circle 50 "solid" "green")) true)

;(define (square? img) false)

(@template-origin Image)

(@template
 (define (square? img)
   (... img)))

(define (square? img)
  (if (= (image-width img) (image-height img))
      true
      false))

;; HtDF Lab, Problem 2

;; PROBLEM:
;; 
;; Design a function named underline that consumes an image 
;; and produces that image underlined by a black line of the same width. 
;; 
;; 
;; For example, 
;; 
;;   (underline (circle 20 "solid" "green"))
;; 
;; should produce
;;
;;   (above (circle 20 "solid" "green")
;;          (rectangle 40 2 "solid" "black"))
(@problem 2)
(@htdf underline) ;!!!UNCOMMENT this line when you start on this function
(@signature Image -> Image)
;; produces an image underlined by a black line when given an image

(check-expect (underline (rectangle 30 20 "solid" "yellow"))
              (above (rectangle 30 20 "solid" "yellow")
                     (rectangle 30 2 "solid" "black")))

(check-expect (underline (rectangle 50 60 "solid" "green"))
              (above (rectangle 50 60 "solid" "green")
                     (rectangle 50 2 "solid" "black")))

(check-expect (underline (rectangle 30 2 "solid" "black"))
              (above (rectangle 30 2 "solid" "black")
                     (rectangle 30 2 "solid" "black")))

;(define (underline imag) (rectangle 30 2 "solid" "black"))

(@template-origin Image)

(@template
 (define (underline imag)
   (... imag)))

(define (underline imag)
  (above imag
         (rectangle (image-width imag) 2 "solid" "black")))

;; HtDF Lab, Problem 3

;; PROBLEM:
;; 
;; A (much too) simple scheme for pluralizing words in English is to add an 
;; s at the end unless the word already ends in s.
;; 
;; Design a function that consumes a string, and adds s to the end unless 
;; the string already ends in s.
(@problem 3)
(@htdf pluralize) ;!!!UNCOMMENT this line when you start on this function
(@signature String -> String)

(check-expect (pluralize "cow") "cows")
(check-expect (pluralize "dogs") "dogs")
(check-expect (pluralize "") "")

;(define (pluralize word) "")

(@template-origin String)

(@template
 (define (pluralize word)
   (... word)))

#| (define (pluralize word)
  (if (= (string-length word) 0)
      (string-copy "")
      (if (string=?
           (substring word (- (string-length word) 1) (string-length word)) "s")
          word
          (string-append word "s")))) |#

(define (pluralize word)
  (cond ((= (string-length word) 0) "")
        [(string=? (substring word (- (string-length word) 1)
                              (string-length word)) "s") word]
        [else (string-append word "s")]))
        

;; HtDF Lab, Problem 4

;; PROBLEM:
;; 
;; Design a function called nth-char-equal? that consumes two strings 
;; and a natural and produces true if the strings both have length greater 
;; than n and have the same character at position n.
;; 
;; 
;; Note, the signature for such a function is:
;; 
;; (@signature String String Natural -> Boolean)
;; 
;; 
;; The tag and template for such a function are:
;; 
;; (@template-origin String)
;; 
;; (@template (define (nth-char-equal? s1 s2 n)
;;              (... s1 s2 n)))
(@problem 4)
(@htdf nth-char-equal?) ;!!!UNCOMMENT this line when you start on this function
(@signature String String Natural -> Boolean)

(check-expect (nth-char-equal? "cow" "cat" 0) true)
(check-expect (nth-char-equal? "dog" "cat" 1) false)
(check-expect (nth-char-equal? "" "hi" 0) false)
(check-expect (nth-char-equal? "123" "123" 2) true)
(check-expect (nth-char-equal? "hi hi" "hi hi" 2) true)
(check-expect (nth-char-equal? "HELLO" "hello" 2) false)
(check-expect (nth-char-equal? "dog" "cat" 5) false)
(check-expect (nth-char-equal? "dog" "cat" 2) false)
(check-expect (nth-char-equal? "dog" "cat" 3) false)

;(define (nth-char-equal? st s2 n) true)

(@template-origin String)

(@template
 (define (nth-char-equal? s1 s2 n)
  (... s1 s2 n)))

#| (define (nth-char-equal? s1 s2 n)
  (if (or (> n (string-length s1)) (> n (string-length s2)))
      false
  (if (string=? s1 s2)
      true
      (if (or (= (string-length s1) 0) (= (string-length s2) 0))
          false
          (if (string=? (substring s1 n (+ n 1)) (substring s2 n (+ n 1)))
              true
              false))))) |#

(define (nth-char-equal? s1 s2 n)
  (cond ((or (>= n (string-length s1)) (>= n (string-length s2))) false)
        [(string=? s1 s2) true]
        [(or (= (string-length s1) 0) (= (string-length s2) 0)) false]
        [(string=? (substring s1 n (+ n 1)) (substring s2 n (+ n 1))) true]
        [else false]))


