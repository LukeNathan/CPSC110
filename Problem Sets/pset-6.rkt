;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname pset-06-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR 
;; PARTNER.
;;
(require spd/tags)

(@assignment psets/pset-06); Do not edit or remove this tag

;; If you are:
;;   - A 110 or 107 student replace the first set of '???'s with your cwl.
;;     For problem sets, If you have a partner, please replace the second
;;     set of '???'s with their cwl.  Remember this, it is what you will
;;     do with these @cwl annotations for the whole course.
;;   - A UBC Extended Learning student, replace the first set of ??? with
;;     your email address as confirmed in the email you received from
;;     extended learning.  The handin password is also in that email.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   
(@cwl lnatha01 ???)


(@problem 1)
;; Below is the start of a data definition called Course that represents limited
;; information about UBC courses.  Below there are only two example data.  
;; Please complete this definition by adding constants C110, C213, C313 and C317
;; which are representations of the descendent tree for 110, 213, 313 and 317.  
;; You can find the information you need at
;;  https://cs110.students.cs.ubc.ca/psets/pset-06-image.png
;;
;; NOTE 1: Use the information in the image above, rather than any other source.
;;         We are significantly simplying the information.
;;
;; NOTE 2: Do this very carefully, the autograder wants to see correct results
;;         from the functions you design to operate on this data.
;;
;; NOTE 3: The tree you will make for C110 will be a bit odd because 210 has 110
;;         as a pre-req, and both 213 and 221 have 210 as a pre-req, and313 has
;;         213 AND 221 as a pre-req, and 317 has 213 AND 221 as a pre-req. As a
;;         result, 313 and 317 will both show up twice in your descendent tree
;          for C110. This is okay for this problem set.
;; NOTE 4: Expect this step of the problem set to take you some time.


(@htdd Course ListOfCourse)
(define-struct course (number credits dependents))
;; Course is (make-course Natural Natural ListOfCourse)
;; interp. a course with a course number,
;;         the number of credits the course is worth, and a
;;         list of courses that list this course as a direct pre-requisite

;; ListOfCourse is one of:
;; - empty
;; - (cons Course ListOfCourse)
;; interp. a list of courses

(define LOC0 empty)
(define LOC221 (list (make-course 304 3 LOC0) (make-course 313 3 LOC0)
                     (make-course 314 3 LOC0) (make-course 317 3 LOC0)
                     (make-course 320 3 LOC0) (make-course 322 3 LOC0)))
(define LOC213 (list (make-course 313 3 LOC0) (make-course 317 3 LOC0)))
(define LOC210 (list (make-course 312 3 LOC0) (make-course 311 3 LOC0)
                     (make-course 310 4 (list (make-course 319 4 LOC0)))
                     (make-course 213 4 LOC213) (make-course 221 4 LOC221)))
(define LOC110 (list (make-course 302 3 LOC0) (make-course 303 3 LOC0)
                     (make-course 203 3 LOC0) (make-course 189 1 LOC0)
                     (make-course 210 4 LOC210)))

(define C100 (make-course 100 3 LOC0))
(define C110 (make-course 110 4 LOC110))
(define C213 (make-course 213 4 LOC213))
(define C313 (make-course 313 3 LOC0))
(define C317 (make-course 317 3 LOC0))
(define C210 (make-course 210 4 LOC210))
(define C310 (make-course 310 4 (list (make-course 319 4 LOC0))))
(define C319 (make-course 319 4 LOC0))
(define C221 (make-course 221 4 LOC221))

(define (fn-for-course c)
  (... (course-number c)
       (course-credits c)
       (fn-for-loc (course-dependents c))))

(define (fn-for-loc loc)
  (cond [(empty? loc) (...)]
        [else
         (... (fn-for-course (first loc))
              (fn-for-loc (rest loc)))]))

(@problem 2)
;;
;; Design a function that produces the list of all the course numbers in the
;; course's tree including the given course's number.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;

(@htdf course-numbers--course course-numbers--loc)
(@signature Course -> ListOfNatural)
(@signature ListOfCourse -> ListOfNatural)

;(check-expect (course-numbers--course C100) (list 100))
;(check-expect (course-numbers--course C110) (list 110))
;(check-expect (course-numbers--course C213) (list 213))
;(check-expect (course-numbers--course C313) (list 313))
;(check-expect (course-numbers--course C317) (list 317))

(check-expect (course-numbers--loc LOC0) empty)
(check-expect (course-numbers--course C110) (list 110 302 303 203 189 210
                                                 312 311 310 319 213
                                                 313 317 221
                                                 304 313 314 317 320 322))
;(check-expect (course-numbers--course C210) (list 210 312 311 310 319 213
;                                                 313 317 221
;                                                 304 313 314 317 320 322))
(check-expect (course-numbers--course C213) (list 213 313 317))
;(check-expect (course-numbers--course C221) (list 221 304 313 314 317 320 322))

;(define (course-numbers--course c) 0)
;(define (course-numbers--loc loc) empty)

(@template-origin Course)
(@template
 (define (course-numbers--course c)
  (... (course-number c)
       (course-credits c)
       (course-numbers--loc (course-dependents c)))))

(define (course-numbers--course c)
  (cons (course-number c)
        (course-numbers--loc (course-dependents c))))

(@template-origin ListOfCourse)
(@template
 (define (course-numbers--loc loc)
  (cond [(empty? loc) (...)]
        [else
         (... (course-numbers--course (first loc))
              (course-numbers--loc (rest loc)))])))

(define (course-numbers--loc loc)
  (cond [(empty? loc) empty]
        [else
         (append (course-numbers--course (first loc))
                 (course-numbers--loc (rest loc)))]))
 

(@problem 3)
;;
;; Design a function that takes two arguments: a Course and a Natural, in that
;; order. It produces the list of courses in the tree that are worth that
;; many credits or more.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;

(@htdf course-credits--course course-credits--loc)
(@signature Course Natural -> ListOfCourse)
(@signature ListOfCourse Natural -> ListOfCourse)

(check-expect (course-credits--loc LOC0 3) empty)
(check-expect (course-credits--course C317 4) empty)
(check-expect (course-credits--course C213 3) (list C213 C313 C317))
(check-expect (course-credits--course C110 4)
              (list C110 C210 C310 C319 C213 C221))

;(define (course-credits--course c n) empty)
;(define (course-credits--loc loc n) empty)

(@template-origin Course)
(@template
 (define (course-credits--course c n)
  (... n (course-number c)
         (course-credits c)
         (course-credits--loc n (course-dependents c)))))

(define (course-credits--course c n)
  (if (>= (course-credits c) n)
      (cons c (course-credits--loc (course-dependents c) n))
      (course-credits--loc (course-dependents c) n)))

(@template-origin ListOfCourse)
(@template
 (define (course-credits--loc loc)
  (cond [(empty? loc) (... n)]
        [else
         (... n (course-credits--course n (first loc))
                (course-credits--loc n (rest loc)))])))

(define (course-credits--loc loc n)
  (cond [(empty? loc) empty]
        [else
         (append (course-credits--course (first loc) n)
              (course-credits--loc (rest loc) n))]))

(@problem 4)
;;
;; Design a function that produces the largest course number in the tree.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;

(@htdf course-largest--course course-largest--loc)
(@signature Course -> Natural)
(@signature ListOfCourse -> Natural)

(check-expect (course-largest--loc LOC0) 0)
(check-expect (course-largest--course C100) 100)
(check-expect (course-largest--course C110) 322)
(check-expect (course-largest--course C213) 317)
(check-expect (course-largest--course C313) 313)
(check-expect (course-largest--course C317) 317)

;(define (course-largest--course c) 0)
;(define (course-largest--loc loc) 0)

(@template-origin Course)
(@template
 (define (course-largest--course c)
  (... (course-number c)
       (course-credits c)
       (course-largest--loc (course-dependents c)))))

(define (course-largest--course c)
  (course-largest--loc (course-numbers--course c)))

(@template-origin ListOfCourse)
(@template
 (define (course-largest--loc loc)
   (cond [(empty? loc) (...)]
         [else
          (... (course-largest--course (first loc))
               (course-largest--loc (rest loc)))])))

(define (course-largest--loc loc)
   (cond [(empty? loc) 0]
         [else
          (largest loc)]))

(@htdf largest)
;; finds the largest natural in list of natural

(check-expect (largest empty) 0)
(check-expect (largest (list 3 2 1)) 3)
(check-expect (largest (list 1 3 2)) 3)
(check-expect (largest (list 1 2 3)) 3)
(check-expect (largest (list 3 3 3)) 3)

(define (largest lon)
  (cond [(empty? lon) 0]
        [else
         (if (> (first lon) (largest (rest lon)))
             (first lon)
             (largest (rest lon)))]))      

(@problem 5)
;;
;; Design a function that takes two arguments: a Course and a Natural, in that
;; order. It produces the course in the tree with that course number. If it
;; can't find a course in the given tree with that course number, it signals
;; failure by producing false.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;

(@htdf course-find--course course-find--loc)
(@signature Course Natural -> Course or false)
(@signature ListOfCourse Natural -> Course or false)

(check-expect (course-find--loc LOC0 100)   false)
(check-expect (course-find--course C110 110) C110)
(check-expect (course-find--course C110 319) C319)
(check-expect (course-find--course C110 500) false)
(check-expect (course-find--course C100 317) false)

(@template-origin Course)
(@template
 (define (course-find--course c)
  (... n (course-number c)
         (course-credits c)
         (course-find--loc (course-dependents c)))))

(@template-origin ListOfCourse try-catch)
(@template
 (define (course-find--loc loc)
  (cond [(empty? loc) (... n)]
        [else
         (... n (course-find--course (first loc))
                (course-find--loc (rest loc)))])))

(define (course-find--course c n)
  (if (and (number? (course-number c)) (= (course-number c) n))
         c
         (course-find--loc (course-dependents c) n)))

#;(define (course-find--loc loc n)
  (cond [(empty? loc) false]
        [else
         (if (and (not (false? (first loc)))
                  (= n (course-find--course (first loc) n)))
                (first loc)
                (course-find--loc (rest loc) n))]))

(define (course-find--loc loc n)
  (cond [(empty? loc) false]
        [else
         (if (not (false? (course-find--loc (rest loc) n)))
                          (course-find--loc (rest loc) n)
             (course-find--course (first loc) n))]))
