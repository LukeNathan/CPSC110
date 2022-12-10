;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab-09-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
;; CPSC 110 - Search Lab

(@assignment labs/lab-09)

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

(@htdd Clown)
;; Clown is String
;; interp.  The name of a clown
(define M "Marny")
(define N "Nifty")
(define O "Oscar")
(define P "Porky")
(define R "Rascal")
(define S "Sunny")
(define V "Vinny")

(@dd-template-rules atomic-non-distinct)
(define (fn-for-clown c)
  (... c))

;; On-ground students:
;; PRELAB: Define examples of ChoiceState that correspond to the two scenarios
;; (on the lab page)

(@htdd ChoiceState)
(define-struct chst (picked options))
;; ChoiceState is (make-chst (listof Clown) (listof Clown))
;; interp. (make-chst picked options) represents a moment during selection when:
;; - we have selected all of the clowns in picked
;; - we are still deciding whether to pick or skip the clowns in options
;; INVARIANT: all of the clowns in picks and options are unique.
(define CH0 (make-chst (list)       (list M N O P R S V))) ; no selections yet
(define CH1 (make-chst (list M)     (list O P R S V)))     ; M picked, N skipped
(define CH2 (make-chst (list M P V) (list)))               ; picking is complete

(@dd-template-rules compound ;; 2 fields
                    ref      ;; (listof Clown)
                    ref)     ;; (listof Clown)

(define (fn-for-chst c)
  (... (fn-for-loc (chst-picked c))
       (fn-for-loc (chst-options c))))



;; ========================================================
;; Functions

;; On-ground Students:
;; PRE-LAB:
;; Our main task in lab will be to design a function, called solve,
;; that takes a choice state and tries to find a list of clowns that satisfy
;; the criteria based from our choice state. Start the design of solve by
;; writing its wish list entry.

;; You should also identify which template elements you will need to blend
;; together for the solve function.

;; Online Students:
;; Follow the instructions on edX

(@htdf solve)
(@signature ChoiceState -> (listof Clown) or false)

(check-expect (solve CH0)  (list S R P O M))

(@template-origin bin-tree genrec try-catch encapsulated)
(define (solve cs)
  (cond [(all-crits? (chst-picked cs)) (chst-picked cs)]
        [(empty? (chst-options cs)) false]
        [else
         (local [(define try (next-cs-picked cs))]
           (if (not (false? (solve try)))
               (solve try)
               (solve (next-cs-skipped cs))))]))    

(@htdf next-cs-picked)
(@signature ChoiceState -> (listof ChoiceState))
;;produces picked next choice state

(check-expect (next-cs-picked (make-chst (list N)
                                         (list O R)))
              (make-chst (list O N)
                         (list R)))
(check-expect (next-cs-picked (make-chst empty empty)) false)

(define (next-cs-picked cs)
  (cond [(empty? (chst-options cs)) false]
        [else
         (make-chst (cons (first (chst-options cs)) (chst-picked cs))
                    (rest (chst-options cs)))]))

(@htdf next-cs-skipped) 
(@signature ChoiceState -> (listof ChoiceState))
;;produces skipped next choice state

(check-expect (next-cs-skipped (make-chst (list N)
                                          (list O R)))
              (make-chst (list N)
                         (list R)))
(check-expect (next-cs-skipped (make-chst empty empty)) false)

(define (next-cs-skipped cs)
  (cond [(empty? (chst-options cs)) false]
        [else
         (make-chst (chst-picked cs)
                    (rest (chst-options cs)))]))










;; Completed Helper Functions that you will use for the lab

(@htdf all-crits?)
(@signature (listof Clown) -> Boolean)
;; produce true if loc satisfies all the clown crits, otherwise false
(check-expect (all-crits? empty) false)
(check-expect (all-crits? (list "Vinny" "Rascal" "Oscar" "Marny")) true)

(@template-origin use-abstract-fn)
(define (all-crits? loc) 
  (andmap (λ (crit?) (crit? loc))
          (list crit-0? crit-1? crit-2? crit-3? crit-4? crit-5?)))

(@htdf crit-0?)
(@signature (listof Clown) -> Boolean)
;; check Criteria 0: There must be at least four clowns
(check-expect (crit-0? empty) false)
(check-expect (crit-0? (list V R)) false)
(check-expect (crit-0? (list V R S O)) true)
(check-expect (crit-0? (list M V R S O)) true)

(@template-origin (listof Clown))
(define (crit-0? loc)
  (>= (length loc) 4))


(@htdf crit-1?)
(@signature (listof Clown) -> Boolean)
;; check Criteria 1: Exactly two of Rascal, Sunny, and Vinny are selected.
(check-expect (crit-1? empty) false)
(check-expect (crit-1? (list R S V)) false)
(check-expect (crit-1? (list V)) false)
(check-expect (crit-1? (list V R)) true)

(@template-origin (listof Clown))
(define (crit-1? loc)
  (or (and (member? R loc)
           (member? S loc)
           (not (member? V loc)))
      (and (member? R loc)
           (member? V loc)
           (not (member? S loc)))
      (and (member? S loc)
           (member? V loc)
           (not (member? R loc)))))


(@htdf crit-2?)
(@signature (listof Clown) -> Boolean)
;; check Criteria 2: Either Nifty is selected, Oscar is selected, or both
(check-expect (crit-2? empty) false)
(check-expect (crit-2? (list N)) true)
(check-expect (crit-2? (list N O)) true)
(check-expect (crit-2? (list M O)) true)

(@template-origin (listof Clown))
(define (crit-2? loc)
  (if (not (member? N loc))
      (member? O loc)
      true))


(@htdf crit-3?)
(@signature (listof Clown) -> Boolean)
;; check Criteria 3: Oscar cannot be selected unless Rascal is selected.
(check-expect (crit-3? empty) true)
(check-expect (crit-3? (list O)) false)
(check-expect (crit-3? (list O R)) true)

(@template-origin (listof Clown))
(define (crit-3? loc)
  (if (member? O loc)
      (member? R loc)
      true))

(@htdf crit-4?)
(@signature (listof Clown) -> Boolean)
;; check Criteria 4: Porky cannot be selected unless Sunny is selected.
(check-expect (crit-4? empty) true)
(check-expect (crit-4? (list P)) false)
(check-expect (crit-4? (list P S)) true)

(@template-origin (listof Clown))
(define (crit-4? loc)
  (if (member? P loc)
      (member? S loc)
      true))

(@htdf crit-5?)
(@signature (listof Clown) -> Boolean)
;; check Criteria 5: If Marny is selected, then Nifty cannot be selected.
(check-expect (crit-5? empty) true)
(check-expect (crit-5? (list M)) true)
(check-expect (crit-5? (list N)) true)
(check-expect (crit-5? (list M N)) false)

(@template-origin (listof Clown))
(define (crit-5? loc)
  (if (member? M loc)
      (not (member? N loc))
      true))
