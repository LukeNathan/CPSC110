;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname handin-2022-12-08T21_04_51) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR 
;; PARTNER.
;;
(require spd/tags)

(@assignment psets/pset-11); Do not edit or remove this tag

;; Replace the first set of '???'s with your cwl.
;; If you have a partner, replace the second set of '???'s with their cwl.
;;   
(@cwl lnatha01 ???)

(@problem 1)
;;
;; In this problem set you will be working with a simple representation of a
;; secret castle.  Unsurpringly, the rooms and doors in this castle form a
;; graph. The figure in:
;;  https://cs110.students.cs.ubc.ca/psets/pset-11-image.png
;; shows a small castle with 5 rooms named A, B, C, D, and E.  A has exits
;; that lead to rooms B, D and E. B has a  single exit that leads to C, and
;; so on.  The ovals (long circles) are locks; the number in the oval is the
;; number of the key required to open that lock. The underlined numbers are
;; keys.
;;
;; E has a lock that requires key # 1 to open.  The lock at room D requires key
;; # 2 to open it. After you enter a room you automatically pickup any keys
;; that are there.  So after you get into room B you automatically pickup key 2.
;; After you get into room C you automatically pickup key 1.
;;
;; Note that in general a door might have multiple locks, and a room might
;; provide multiple keys.
;;
;; Here are the data definitions we use.

;; Data definitions:

(@htdd Room)
(define-struct room (name locks keys exits))
;; Room is (make-room String (listof Natural) (listof Natural) (listof String))
;; interp. each room has
;;           - a Name
;;           - locks that require a key of the same number to open
;;           - keys that open locks of the same number
;;           - doors to other rooms, these are represented as a list
;;             of strings where each string is the name of a room
;;
;; NOTE: The keys can be for any rooms in the castle, they do not have
;;       to be for one of the rooms in exits.
;;
;; This is a generative graph problem. Note that a room has a list of the names
;; of the rooms it has exits to -- the names not the actual rooms.  Therefore a
;; generative step has to take the name and generate the actual Room. To do
;; that we add a new opaque data definition for a type called Map.  When
;; we say that Map is opaque we mean that you should not try to look inside
;; of it.  It is a secret map of the castle. Instead we are providing a function
;; called get-room that given a room name and a castle will generate the room.
;;

(@htdd Map)
;; Map is ???
;; interp. an opaque data type that represents a map from room names to rooms.
;;         Only the provided function get-room knows how to work with a map.
;;

#|
Here is a partially blended template. It includes the Room and (listof String) 
functions, as well as the generative step of calling get-room to get a room from
a room name. But it doesn't include the trivial? test to terminate the
generative recursion or anything to prevent going in cycles.  You must complete
the blending for your own purposes.

(define (fn-for-castle start castle)
  (local [(define (fn-for-room r)
            (... (room-name r)
                 (room-locks r)
                 (room-keys r)
                 (fn-for-los (room-exits r))))

          (define (fn-for-los los)
            (cond [(empty? los) (...)]
                  [else
                   (... (fn-for-room (get-room (first los) castle))
                        (fn-for-los (rest los)))]))]

    (fn-for-room (get-room start) castle)))

|#

;;
;; Design a function that consumes a start room name (String), a to room
;; name (String), and a Map of the castle and tries to find a path from start to
;; to in the given castle. A path:
;;   - cannot go into a room more than once
;;   - cannot go into a room unless the required keys are being held
;; You can only pick up the keys in a room once you are inside it.  You may
;; carry more than one key at a time.
;;
;; Using the map MAP, one legal path from A to the room named E is A, B, C, D, E
;;

;;
;; 
;; If a path is possible your function should produce the list of room names
;; traversed in order.
;;    (find-path A "E" MAP) should produce (list "A" "B" "C" "D" "E")
;;    (find-path D "E" MAP) should produce false
;;
;;
;; Your function should NOT use tail recursion.  Just use ordinary structural
;; recursion.  We will mark solutions that use tail recursion, but the function
;; is harder to write that way so we advise against it.
;;
;; REMEMBER:
;;  - THE MAP IS AN OPAQUE DATA STRUCTURE. THE ONLY THING YOU ARE
;;    ALLOWED TO DO WITH IT IS PASS IT TO GET-ROOM.
;;  - As always, your function definition must work for any Map (any
;;    such graph). We will test it with different examples than this one.
;;

(@htdf find-path)
(@signature String String Map -> (listof String) or false)
;; try to find path between from and to, while enforcing door lock rules

(check-expect (find-path "A" "E" MAP) (list "A" "B" "C" "D" "E"))
(check-expect (find-path "D" "E" MAP) false)
(check-expect (find-path "C" "D" MAP) false)
(check-expect (find-path "C" "E" MAP) (list "C" "A" "E"))
(check-expect (find-path "A" "D" MAP) (list "A" "B" "C" "D")) 

(@template-origin accumulator try-catch encapsulated Room
                  (listof String) genrec)
(define (find-path start to castle)
  ;; path is (listof String), the travel path
  ;; lok is (listof Natural), list of currently held keys
  (local [(define (fn-for-room r path lok)
            (local [(define name (room-name r)) 
                    (define keys (room-keys r))
                    (define exits (room-exits r))]
              
              (if (string=? name to)
                  (reverse (cons name path)) 
                  (fn-for-los exits (cons name path) (cons keys lok)))))
               
          (define (fn-for-los los path lok)  
            (cond [(empty? los) false]   
                  [(member? (first los) path)
                   (fn-for-los (rest los) path lok)]   
                  [(and (not (empty?
                              (room-locks (get-room (first los) castle))))
                        (not (member?
                              (room-locks (get-room (first los) castle)) lok)))
                   (fn-for-los (rest los) path lok)] 
                  [else
                   (local [(define try (fn-for-room
                                        (get-room (first los) castle)
                                        path lok))]
                     (if (not (false? try))
                         try
                         (fn-for-los (rest los) path lok)))]))]
    
    (if (not (empty? (room-locks (get-room start castle)))) 
        false
        (fn-for-room (get-room start castle) empty empty))))

;; DO NOT READ BELOW HERE
;;
;; Call get-room with a room name and a castle to get that room. Treat this
;; as an opaque primitive function - in other words, don't look at the code for
;; its definition.
;;
(define (get-room name castle)
  (local [(define alist (with-input-from-string castle read))
          (define entry (assoc name alist))]
    (if (false? entry)
        (error "Room with given name does not exist in Castle.")
        (apply make-room entry))))


(define MAP
  "((\"A\"()()(\"B\"\"D\"\"E\"))(\"B\"()(2)(\"C\"))
    (\"C\"()(1)(\"A\"\"D\"))(\"D\"(2)()(\"B\"\"E\"))(\"E\"(1)()())))")

