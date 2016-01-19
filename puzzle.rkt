;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname puzzle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following line is REQUIRED (do not remove)
(require "puzlib.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Place your Personal Identification here
;;  Yihan Wang (20610851)
;;  CS 135, Fall 2015
;;  Assignment 10, Problem 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; A Puzzle is a (list (listof String) (listof String))

;; A Grid is a (listof (listof Char))

(define-struct wpos (row col horiz? len))
;; A WPos (Word Position) is a (make-wpos Nat Nat Bool Nat)
;; requires: len > 1

(define-struct state (grid positions words))
;; A State is a (make-state Grid (listof WPos) (listof Str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS FOR TESTING:

(define puzz01 (read-puzzle "puzzle01.txt"))
(define grid-abc '((#\A #\B #\C) (#\X #\Y #\Z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED HELPER:

;; (flip wp) transposes wp by reversing row/col and negating horiz?
;; flip: WPos -> WPos
;; Example:
(check-expect (flip (make-wpos 3 4 true 5))
              (make-wpos 4 3 false 5))
 
(define (flip wp)
  (make-wpos (wpos-col wp) (wpos-row wp) (not (wpos-horiz? wp)) (wpos-len wp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REQUIRED FUNCTIONS:


;; (transpose g) produces a transposed grid by switching the row and column of each element in g
;; transpose: Grid -> Grid
;; requires: Grid is non-empty, Grid is a rectangle
;; Examples: 
(check-expect (transpose grid-abc) '((#\A #\X) (#\B #\Y) (#\C #\Z)))
(check-expect (transpose (list (list 1 2 3) (list 4 5 6))) (list (list 1 4) (list 2 5) (list 3 6)))

(define (transpose g)  
  (cond [(empty? g) empty]
        [(empty? (first g)) empty]
        [else (cons (map (lambda (x) (first x)) g) 
                    (transpose (map (lambda (y) (remove (first y) y)) g)))]))

;; Tests:
(check-expect (transpose empty) empty)
(check-expect (transpose (list empty empty)) empty) 
(check-expect (transpose (list (list 1) (list 2) (list 3))) (list (list 1 2 3)))
(check-expect (transpose (list (list 1 2) (list 3 4))) (list (list 1 3) (list 2 4)))
(check-expect (transpose (list (list 1 2 3 4) (list 2 3 4 5))) (list (list 1 2) (list 2 3) (list 3 4) (list 4 5)))


;; (find-wpos loc row) produces all possible WPos from loc using row as their row
;; find-wpos: (listof Char) Nat -> (listof WPos)
;; Examples:
(check-expect (find-wpos (string->list "####") 0)
              (list (make-wpos 0 0 true 4)))
(check-expect (find-wpos (string->list "##..#") 1)
              (list (make-wpos 1 0 true 2)))

(define (find-wpos loc row) 
  (local 
    [;; (location lst index) produces a list of numbers representing the locations of #s
     ;; location: (listof Char) Nat -> (listof Nat)
     (define (location lst index)
       (cond [(empty? lst) empty]
             [(char=? #\# (first lst)) (cons index (location (rest lst) (add1 index)))]
             [else (location (rest lst) (add1 index))]))
     ;; (first-sublist lst check) produces a list of number which is the first set of
     ;; consecutive numbers from lst
     ;; first-sublist: (listof Nat) Nat -> (listof Nat)
     (define (first-sublist lst check)
       (cond [(empty? lst) empty] 
             [(not (= (first lst) check)) empty]
             [else (cons (first lst) (first-sublist (rest lst) (add1 check)))]))
     ;; (removed lst1 my-lst) produces a new list with lst1 removed form my-lst
     ;; removed (listof Nat) (listof Nat) -> (listof Nat)
     (define (removed lst1 my-lst)
       (cond [(empty? lst1) my-lst]
             [(member? (first lst1) my-lst) (removed (rest lst1) (remove (first lst1) my-lst))]))
     ;; (sublist lst) produces a list of sublist with each sublist being a set of consecutive numbers
     ;; sublist: (listof Nat) -> (listof (listof Nat))
     (define (sublist lst)
       (cond [(empty? lst) empty]
             [else (cons (first-sublist lst (first lst)) (sublist (removed (first-sublist lst (first lst))lst)))]))
     ;; (produce lst row) converts lst to a list of WPos
     ;; produce: (listof Nat) Nat -> (listof Wpos)
     (define (produce lst row)
       (cond [(empty? lst) empty]
             [else (cons (make-wpos row (first (first lst)) true (length (first lst)))
                         (produce (rest lst) row))]))
     (define final-lst (produce (sublist (location loc 0)) row))]
   (filter (lambda (x) (> (wpos-len x) 1)) final-lst)))

;; Tests:
(check-expect (find-wpos empty 0) empty)
(check-expect (find-wpos empty 1) empty)
(check-expect (find-wpos (string->list "##.##") 0)
              (list (make-wpos 0 0 true 2)
                    (make-wpos 0 3 true 2)))
(check-expect (find-wpos (string->list ".....") 9) empty)                      
(check-expect (find-wpos (string->list "###") 5)
              (list (make-wpos 5 0 true 3)))
(check-expect (find-wpos (string->list "..####..") 5)
              (list (make-wpos 5 2 true 4)))
;; the order does not matter: here is an example
;; that uses lists-equiv?
(check-expect (lists-equiv?
               (find-wpos (string->list "..####...###..") 5)
               (list (make-wpos 5 2 true 4)
                     (make-wpos 5 9 true 3)))
              true)
(check-expect (find-wpos (string->list "#.#..#.#") 5)
              empty)
(check-expect (find-wpos empty 9) empty)


;; (initial-state puzzle) produces the initial state from puzzle
;; initial-state: Puzzle -> State
;; Examples:
(check-expect (initial-state puzz01)
              (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))
(check-expect (initial-state (list (list "#####" "####." "#....")
                                   (list "abcde" "math" "h")))
              (make-state (list (string->list "#####") (string->list "####.") (string->list "#...."))
                          (list (make-wpos 0 0 true 5)
                                (make-wpos 1 0 true 4)
                                (make-wpos 0 0 false 3)
                                (make-wpos 0 1 false 2)
                                (make-wpos 0 2 false 2) 
                                (make-wpos 0 3 false 2))
                          (list "abcde" "math" "h")))
                                                                           
(define (initial-state puzzle)
  (local
    [(define g (map (lambda (x) (string->list x)) (first puzzle)))
     (define (positions g n) 
       (cond [(empty? g) empty]
             [else (append (find-wpos (first g) n)
                           (positions (rest g) (add1 n)))]))]
    (make-state g (append (positions g 0)
                          (map flip (positions (transpose g) 0))) (second puzzle))))

;; Tests:
(check-expect (initial-state (list  (list "####") (list "math")))
              (make-state (list (string->list "####"))
                          (list (make-wpos 0 0 true 4))
                          (list "math")))
(check-expect (initial-state (list (list "###" "#.#" "...") (list "wat" "er" "lo")))
              (make-state (list (string->list "###") (string->list "#.#") (string->list "..."))
                          (list (make-wpos 0 0 true 3)
                                (make-wpos 0 0 false 2)
                                (make-wpos 0 2 false 2))
                          (list "wat" "er" "lo")))


;; (extract-wpos g wp) produces the corresponding chars from wp in g
;; extract-wpos: Grid WPos -> (listof Char)
;; Examples: 
(check-expect (extract-wpos grid-abc (make-wpos 0 0 true 2)) '(#\A #\B))
(check-expect (extract-wpos grid-abc (make-wpos 0 0 false 2)) '(#\A #\X))
(check-expect (extract-wpos grid-abc (make-wpos 0 2 false 2)) '(#\C #\Z))

(define (extract-wpos g wp)
  (local
    [;; (find-row wp g n) produces the whole row where wp occurs form g
     ;; find-row: WPos Grid Nat -> (listof Char)
     (define (find-row wp g n)  
       (cond [(empty? g) empty]
             [(= (wpos-row wp) n) (first g)]
             [else (find-row wp (rest g) (add1 n))]))
     ;; (find-col wp lst n) produces the row from where wp occurs from g
     ;; find-col: WPos (listof Char) Nat -> (listof Char)
     (define (find-col wp lst n) 
       (cond [(empty? lst) empty] 
             [(= (wpos-col wp) n) lst]
             [else (find-col wp (rest lst) (add1 n))]))
     ;; (find-len wp lst n) produces the chars corresponding to wp from g
     ;; find-len: WPos  (listof Char) Nat -> (listof Char)
     (define (find-len wp lst n)
             (cond [(empty? lst) empty]
                   [(= (wpos-len wp) n) empty]
                   [else (cons (first lst) (find-len wp (rest lst) (add1 n)))]))
     (define wp-v (flip wp))]
  (cond [(wpos-horiz? wp) (find-len wp (find-col wp (find-row wp g 0) 0) 0)]
        [else (find-len wp-v (find-col wp-v (find-row wp-v (transpose g) 0) 0) 0)])))

;; Tests:
(check-expect (extract-wpos empty (make-wpos 0 0 true 3)) empty)
(check-expect (extract-wpos (list empty empty) (make-wpos 0 0 true 2)) empty)
(check-expect (extract-wpos (list (string->list "####") (string->list "##..") (string->list "#..#"))
                            (make-wpos 0 0 true 4))
              (string->list "####"))
(check-expect (extract-wpos (list (string->list "####") (string->list "##..") (string->list "A..#"))
                            (make-wpos 0 0 false 3))
              (string->list "##A"))
(check-expect (extract-wpos (list (string->list "####") (string->list "##..") (string->list "A..#"))
                            (make-wpos 0 1 false 2))
              (string->list "##"))
(check-expect (extract-wpos (list (string->list ".##.")) (make-wpos 0 1 true 2))
              (string->list "##"))


;; (replace-wpos g wp loc) replaces the chars in g with loc based on wp
;; replace-wpos: Grid WPos (listof Char) -> Grid
;; requires: len in WPos is equal to length of (listof Char)
;; Examples:
(check-expect (replace-wpos grid-abc (make-wpos 0 0 true 2) '(#\J #\K))
              '((#\J #\K #\C) (#\X #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 0 false 2) '(#\J #\K))
              '((#\J #\B #\C) (#\K #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 2 false 2) '(#\J #\K))
              '((#\A #\B #\J) (#\X #\Y #\K)))

(define (replace-wpos g wp loc)
  (local
    [;; (removed len my-lst) remove the first len element from my-lst
     ;; removed: Nat (listof Char) -> (listof Char)
     (define (removed len my-lst)
       (cond [(empty? my-lst) empty]
             [(= len 0) my-lst]
             [else (removed (sub1 len) (rest my-lst))]))
     ;; (replace my-lst wp loc m) replaces elements in my-lst with corresponding char in loc
     ;;    based on wp
     ;; replace: (listof Char) WPos (listof Char) Nat -> (listof Char)
     (define (replace my-lst wp loc m)
       (cond [(empty? my-lst) empty] 
             [(not (= m (wpos-col wp))) (cons (first my-lst)
                                              (replace (rest my-lst) wp loc (add1 m)))]
             [else (append loc (removed (wpos-len wp) my-lst))]))
     ;; (readin g wp loc n) produces a new grid called g with corresponding elements replaced
     ;;     by elemtents in loc based on wp
     ;; readin: Grid WPos (listof Char) Nat -> Grid
     (define (readin g wp loc n)
       (cond [(empty? g) empty] 
             [(not (= (wpos-row wp) n)) (cons (first g) (readin (rest g) wp loc (add1 n)))]
             [else (cons (replace (first g) wp loc 0)
                                        (readin (rest g) wp loc (add1 n)))]))]
  (cond [(empty? g) empty]
        [(wpos-horiz? wp) (readin g wp loc 0)]
        [else (transpose (readin (transpose g) (flip wp) loc 0))])))

;; Tests:
(check-expect (replace-wpos empty (make-wpos 0 0 true 2) (string->list "ABC")) empty)
(check-expect (replace-wpos (list empty empty) (make-wpos 0 0 true 2)
                             (string->list "ABC")) (list empty empty))
(check-expect (replace-wpos (list (string->list "####"))(make-wpos 0 0 true 4) (string->list "ABCD"))
              (list (string->list "ABCD")))
(check-expect (replace-wpos (list (string->list "####") (string->list "##..") (string->list "A..#"))
                            (make-wpos 0 0 false 3) (string->list "ABA"))
              (list (string->list "A###") (string->list "B#..") (string->list "A..#")))

              


;; (fit? word cells) determines whether a wrod can successfully be placed in the
;;     corresponding word position.
;; fit? (listof Char) (listof Char) -> Bool
;; Examples:
(check-expect (fit? (string->list "STARWARS") (string->list "S##RW##S")) true)
(check-expect (fit? (string->list "STARWARS") (string->list "S##RT##K")) false)

(define (fit? word cells)
  (cond [(not (= (length word) (length cells))) false]
        [(empty? word) true]
        [(or (char=? (first word) (first cells))
             (char=? (first cells) #\#)) (fit? (rest word) (rest cells))]
        [else false]))
        
;; Tests:
(check-expect (fit? empty empty) true)
(check-expect (fit? empty (list #\A)) false)
(check-expect (fit? (list #\a #\b) (list #\# #\#)) true)
(check-expect (fit? (list #\a #\b) (list #\a #\b)) true)
(check-expect (fit? (list #\a #\b) (list #\# #\b)) true)
(check-expect (fit? (list #\a #\b) (list #\V #\b)) false)
                          

;; (neighbours s) produces a list of States based on the original state called s
;; neighbours: State -> (listof State)
;; Examples:
(check-expect (neighbours (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))
              (list (make-state '((#\C #\A #\T)) empty empty)))

(define (neighbours s)
  (local
    [;; (wordlst lst g wp) produces a list of words which can be fitted in a cell from
     ;;    lst g and wp
     ;; wordlst: (listof Str) Grid WPOS -> (listof Str)
     (define (wordlst lst g wp)
       (cond [(empty? lst) empty]
             [(fit? (string->list (first lst)) (extract-wpos g wp))
              (cons (first lst) (wordlst (rest lst) g wp))]
        [else (wordlst (rest lst) g wp)]))
     ;; (gridlst wordlst g wp) produces a list of possible grid from a list of words called
     ;;  wordlst and a grid g and a wpos called wp
     ;; gridlst: (listof Str) Grid WPOS -> (listof Grid)
     (define (gridlst wordlst g wp)
       (cond [(empty? wordlst) empty]
             [else (cons (replace-wpos g wp (string->list (first wordlst)))
                         (gridlst (rest wordlst) g wp))]))
     ;; (one-statelst gridlst wordlst wp s) uses wp and s to build a list of states based on
     ;;   gridlst and wordlst
     ;; one-statelst: (listof Grid) (listof String) WPos State -> (listof State)
     (define (one-statelst gridlst wordlst wp s)
       (cond [(empty? gridlst) empty]
             [else (cons (make-state (first gridlst)
                                     (remove wp (state-positions s))
                                     (remove (first wordlst) (state-words s)))
                   (one-statelst (rest gridlst) (rest wordlst) wp s))]))
     ;; (one-neighbour wp s) wraps one-statelst, gridlst and worlst using wp and s to one function
     ;; one-neighbour: WPos State -> (listof State)
     (define (one-neighbour wp s)
       (one-statelst (gridlst (wordlst (state-words s) (state-grid s) wp)
                              (state-grid s) wp)
                     (wordlst (state-words s) (state-grid s) wp)
                      wp s))
     ;; (my-neighbours wplst s) produces a list of states that will happen give an initial state s
     ;; my-neighbours: (listof WPos) State -> (listof State)
     (define (my-neighbours wplst s)
       (cond [(empty? wplst) empty]
             [else (append (one-neighbour (first wplst) s)
                           (my-neighbours (rest wplst) s))]))]
   (my-neighbours (state-positions s) s)))


;; Tests:
(check-expect (neighbours (make-state '((#\C #\# #\#))
                                      (list (make-wpos 0 0 true 3))
                                      '("CAT" "DOG" "CAR")))
              (list (make-state '((#\C #\A #\T)) empty '("DOG" "CAR"))
                    (make-state '((#\C #\A #\R)) empty '("CAT" "DOG"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED FUNCTIONS:

;; (solved? s) determines if s is a solved criss-cross problem
;;   by checking if all of the word positions have been filled
;; solved?: State -> Bool
(define (solved? s)
  (empty? (state-positions s)))


;; (criss-cross puzzle) produces a list of strings corresponding
;;   to the solution of the the criss-cross puzzle,
;;   or false if no solution is possible
;; criss-cross: Puzzle -> (anyof false (listof Str))

(define (criss-cross puzzle)
  (local [(define result (solve (initial-state puzzle)
                                neighbours
                                solved?))]
    (cond [(false? result) false]
          [else (map list->string (state-grid result))])))

;;(check-expect (criss-cross puzz01) '("CAT"))

;; note that [solve] (used in criss-cross above) is provided in puzlib.rkt

;; when you are all done, you can use disp to
;; view your solutions:

;;(disp (criss-cross (read-puzzle "puzzle05.txt")))

;; NOTE: Do NOT leave top-level expressions in your code.
;;       In other words, when your code is run, only the
;;       check-expect message "All X tests passed!"
;;       should appear in the interactions window

