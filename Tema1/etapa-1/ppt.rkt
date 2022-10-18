#lang racket

(provide (all-defined-out))

;; Un triplet pitagoreic primitiv (TPP) este format din 
;; 3 numere naturale nenule a, b, c cu proprietățile:
;;    a^2 + b^2 = c^2
;;    a, b, c prime între ele
;;
;; TPP pot fi generate sub formă de arbore (infinit) cu
;; rădăcina (3,4,5), pe baza a 3 transformări matriciale:
;;
;;      |-1 2 2|        |1 2 2|        |1 -2 2|
;; T1 = |-2 1 2|   T2 = |2 1 2|   T3 = |2 -1 2|
;;      |-2 2 3|        |2 2 3|        |2 -2 3|
;;
;;                         (3,4,5)
;;              ______________|______________
;;             |              |              |
;;         (15,8,17)      (21,20,29)     (5,12,13)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;; (35,12,37) ..........................................
;;
;; unde:
;; (15, 8,17) = T1·(3,4,5)
;; (21,20,29) = T2·(3,4,5)
;; ( 5,12,13) = T3·(3,4,5) etc.
;;
;; În această reprezentare, TPP sunt indexate "de sus în jos",
;; respectiv "de la stânga la dreapta", rezultând ordinea:
;; (3,4,5) (15,8,17) (21,20,29) (5,12,13) (35,12,37) ... etc.

;; Reprezentăm matricile T1, T2, T3 ca liste de liste:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Implementați o funcție care calculează produsul scalar
; a doi vectori X și Y (reprezentați ca liste).
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
; Utilizați recursivitate pe stivă.
(define (dot-product X Y)
  (cond ((null? X) 0)
        (else
        (+ (* (car X) (car Y))
           (dot-product (cdr X) (cdr Y)))))
  )


; TODO
; Implementați o funcție care calculează produsul dintre
; o matrice M și un vector V (puneți V "pe verticală").
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
; Utilizați recursivitate pe coadă.
(define (multiply M V)
  (cond ((null? M) null)
        (else
        (append (list (dot-product (car M) V))
           (multiply (cdr M) V))))
  )


; TODO
; Implementați o funcție care primește un număr n și
; întoarce o listă numerică (unde elementele au valoarea
; 1, 2 sau 3), reprezentând secvența de transformări prin
; care se obține, plecând de la (3,4,5), al n-lea TPP
; din arbore.
; Ex: (get-transformations 8) întoarce '(2 1), adică
; al 8-lea TPP din arbore se obține din T1·T2·(3,4,5).
; Sunteți încurajați să folosiți funcții ajutătoare
; (de exemplu pentru determinarea nivelului din arbore 
; pe care se află n, sau a indexului minim/maxim de pe 
; nivelul respectiv, etc.)

;calculez nivelul la care se afla n
(define (get-level n level sum)
  (cond ((<= n (+ sum (la-putere level 1))) level)
        (else
        (get-level n (+ level 1) (+ sum (la-putere level 1)))))
  )

;calculeaza cate numere sunt pana la nivelul lui n
(define (get-sum n level sum)
  (cond ((<= n sum) sum)
        (else
        (get-sum n (+ level 1) (+ sum (la-putere level 1)))))
  )

;calculez 3 la puterea x
(define (la-putere x rez)
  (cond ((equal? x 0) rez)
        (else
        (la-putere (- x 1) (* rez 3))))
  )

;rastoarna lista data ca parametru
(define (rev L)
  (if (null? L)
      L
      (append (rev (cdr L))
              (list (car L))))
  )

;aflu in care secventa de nivel se afla n si printez
;numarul aferent acesteia
(define (get-list n minn maxx L) 
  (cond ((< (- maxx minn) 2) L)
        ((= 1 n) L)
        ((<= n (+ minn (/ (- maxx minn) 3)))
         (get-list n minn (- maxx (* 2 (/ (- maxx minn) 3))) (append '(1) L)))
        ((<= n (+ minn (* 2 (/ (- maxx  minn) 3))))
         (get-list n (+ minn (/ (- maxx minn) 3)) (- maxx (/ (- maxx minn) 3)) (append '(2) L)))
        (else
        (get-list n (+ minn (* 2 (/ (- maxx minn) 3))) maxx (append '(3) L))))
  )

(define (get-transformations n)
  (rev (get-list n (- (get-sum n 0 0) (la-putere (get-level n 0 0) 1)) (get-sum n 0 0) '()))
  )


; TODO
; Implementați o funcție care primește o listă Ts de 
; tipul celei întoarsă de get-transformations, respectiv 
; un triplet de start ppt și întoarce tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Utilizați recursivitate pe coadă.

(define (tail-recursion Ts acc)
  (cond ((null? Ts) acc)
        ((= 1 (car Ts)) (tail-recursion (cdr Ts) (multiply T1 acc)))
        ((= 2 (car Ts)) (tail-recursion (cdr Ts) (multiply T2 acc)))
        (else
        (tail-recursion (cdr Ts) (multiply T3 acc))))
  )

(define (apply-matrix-transformations Ts ppt)
  (tail-recursion Ts ppt)
  )
 
; TODO
; Implementați o funcție care calculează al n-lea TPP
; din arbore, folosind funcțiile anterioare.
(define (get-nth-ppt-from-matrix-transformations n)
  (apply-matrix-transformations (get-transformations n) '(3 4 5))
 )