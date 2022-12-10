;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname lab-06-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(@assignment labs/lab-06)

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

(@problem 1)
;; DATA DEFINITIONS ===============

(@htdd SentenceTree)
(define-struct stree (prefix subs))
;; SentenceTree is (make-stree String ListOfSentenceTree)
;; interp. a node of a Sentence Tree

(define ST0 empty)
(define ST2 (make-stree "JOKING ABOUT JEALOUSY" empty))
(define ST4 (make-stree "WE ARE" (list
             (make-stree "IN A BACK TO SCHOOL SPECIAL ABOUT MONO" empty)
             (make-stree "PERCHED ON THE TIP OF A SINKING SHIP" empty))))
(define ST3 (make-stree "LIKE" (list (make-stree "YOU REALLY MEAN IT" empty)
                                     ST4)))
(define ST5 (make-stree "TO" (list (make-stree "FREEZE TIME" empty)
                                   (make-stree "MY FAVOURITE SONG ON REPEAT"
                                               empty))))
(define ST1 (make-stree "KISS ME" (list ST2 ST3 ST5)))

(@htdd ListOfSentenceTree)
;; ListOfSentenceTree is one of:
;; empty
;; (cons SentenceTree ListOfSentenceTree)
;; interp. a SentenceTree
(@template-origin SentenceTree)

(define (fn-for-stree st)
  (... (stree-prefix st)
       (fn-for-lostree (stree-subs st))))

(@template-origin ListOfSentenceTree)

(define (fn-for-lost lost)
  (cond [(empty? lost) (...)]
        [else
         (... (fn-for-stree (first lost))
              (fn-for-lost (rest lost)))]))

;; FUNCTIONS ======================
(@problem 2)

(@htdf sentence-count--stree sentence-count--lostree)
(@signature SentenceTree -> Natural)
(@signature ListOfSentenceTree -> Natural)
;; count number of sentences on a SentenceTree

(check-expect (sentence-count--stree ST1) 6)
(check-expect (sentence-count--stree ST2) 1)
(check-expect (sentence-count--stree ST3) 3)
(check-expect (sentence-count--stree ST4) 2)
(check-expect (sentence-count--stree ST5) 2)

(@template-origin SentenceTree)

(define (sentence-count--stree st)
  (if (not (empty? (stree-subs st)))
      (sentence-count--lostree (stree-subs st))
      (+ 1 (sentence-count--lostree (stree-subs st)))))

(@template-origin ListOfSentenceTree)

(check-expect (sentence-count--lostree ST0) 0)

(define (sentence-count--lostree lost)
  (cond [(empty? lost) 0]
        [else
         (+ (sentence-count--stree (first lost))
              (sentence-count--lostree (rest lost)))]))

(@problem 3)

(@htdf render--stree render--lost)
(@signature SentenceTree -> Image)
(@signature ListOfSentenceTree -> Image)
;; render a sentence tree

(check-expect (render--stree ST2)
              (text "JOKING ABOUT JEALOUSY" TEXT-SIZE TEXT-COLOUR))

(define TEXT-SIZE 15)
(define TEXT-COLOUR "black")

(@template-origin SentenceTree)

(define (render--stree st)
  (beside (text (stree-prefix st) TEXT-SIZE TEXT-COLOUR)
           (render--lost (stree-subs st))))

(@template-origin ListOfSentenceTree)

(define (render--lost lost)
  (cond [(empty? lost) empty-image]
        [else
          (above/align "left" (render--stree (first lost))
   	           (render--lost (rest lost)))]))
                  