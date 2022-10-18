#lang racket

(provide (all-defined-out))

;; Același arbore de TPP obținut în etapa 1 prin aplicarea
;; transformărilor T1, T2, T3 poate fi generat folosind 
;; tupluri GH (Gopal-Hemachandra).
;;
;; Pentru o pereche oarecare (g, e), secvența GH este:
;;    g, e, g + e, g + 2e, 2g + 3e, 3g + 5e ...
;; Pentru (g, e) = (1, 1) obținem șirul lui Fibonacci.
;;
;; Primele 4 numere din secvență formează cvartetul GH:
;;    (g, e, f, h) = (g, e, g + e, g + 2e)
;;
;; Pentru un asemenea cvartet (g, e, f, h), definim:
;;    a = gh,   b = 2ef,   c = e^2 + f^2
;; și putem demonstra că (a,b,c) este un triplet pitagoreic.
;;
;; (a,b,c) este chiar TPP, dacă adăugăm condițiile:
;;    g, e, f, h prime între ele
;;    g impar
;; însă nu veți avea nevoie să faceți asemenea verificări,
;; întrucât avem la dispoziție un algoritm care generează
;; exclusiv TPP.
;;
;; Acest algoritm este foarte asemănător cu cel din etapa
;; anterioară, cu următoarele diferențe:
;;  - nodurile din arbore sunt cvartete, nu triplete
;;    (din cvartet obținem un TPP conform formulelor)
;;    (ex: (1,1,2,3) => (1*3,2*1*2,1^2+2^2) = (3,4,5))
;;  - obținem următoarea generație de cvartete folosind 
;;    trei transformări Q1, Q2, Q3 pentru cvartete, în loc
;;    de T1, T2, T3 care lucrau cu triplete
;; 
;; Q1(g,e,f,h) = (h,e,h+e,h+2e)
;; Q2(g,e,f,h) = (h,f,h+f,h+2f) 
;; Q3(g,e,f,h) = (g,f,g+f,g+2f)
;;
;; Arborele rezultat arată astfel:
;;
;;                        (1,1,2,3)
;;              ______________|______________
;;             |              |              |
;;         (3,1,4,5)      (3,2,5,7)      (1,2,3,5)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;;  (5,1,6,7) .........................................

;; Definim funcțiile Q1, Q2, Q3:
(define (Q1 g e f h) (list h e (+ h e) (+ h e e)))
(define (Q2 g e f h) (list h f (+ h f) (+ h f f)))
(define (Q3 g e f h) (list g f (+ g f) (+ g f f)))

;; Vom refolosi matricile T1, T2, T3:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Reimplementați funcția care calculează produsul scalar
; a doi vectori X și Y, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
(define (dot-product X Y)
  (foldr + 0 (map (lambda (nr1 nr2) (* nr1 nr2)) X Y))
  )


; TODO
; Reimplementați funcția care calculează produsul dintre
; o matrice M și un vector V, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
(define (multiply M V)
  (map (lambda (nr1) (dot-product nr1 V)) M)
  )


; TODO
; Aduceți aici (nu sunt necesare modificări) implementarea
; funcției get-transformations de la etapa 1.
; Această funcție nu este re-punctată de checker, însă este
; necesară implementărilor ulterioare.
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
; În etapa anterioară ați implementat o funcție care primea
; o listă Ts de tipul celei întoarsă de get-transformations
; și un triplet de start ppt și întorcea tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Acum dorim să generalizăm acest proces, astfel încât să
; putem reutiliza funcția atât pentru transformările de tip
; T1, T2, T3, cât și pentru cele de tip Q1, Q2, Q3.
; În acest scop operăm următoarele modificări:
;  - primul parametru este o listă de funcții Fs
;    (în loc de o listă numerică Ts)
;  - al doilea parametru reprezintă un tuplu oarecare
;    (aici modificarea este doar "cu numele", fără a schimba
;    funcționalitatea, este responsabilitatea funcțiilor din
;    Fs să primească parametri de tipul lui tuple)
; Nu folosiți recursivitate explicită (ci funcționale).
(define (apply-functional-transformations Fs tuple)
  (foldl (lambda (nr1 nr2) (nr1 nr2)) tuple Fs)
  )


; TODO
; Tot în spiritul abstractizării, veți defini o nouă funcție
; get-nth-tuple, care calculează al n-lea tuplu din arbore. 
; Această funcție va putea fi folosită:
;  - și pentru arborele de triplete (caz în care plecăm de la
;    (3,4,5) și avansăm via T1, T2, T3)
;  - și pentru arborele de cvartete (caz în care plecăm de la
;    (1,1,2,3) și avansăm via Q1, Q2, Q3)
; Rezultă că, în afară de parametrul n, funcția va trebui să
; primească un tuplu de start și 3 funcții de transformare a
; tuplurilor.
; Definiți get-nth-tuple astfel încât să o puteți reutiliza
; cu minim de efort pentru a defini funcțiile următoare:
;    get-nth-ppt-from-matrix-transformations
;    get-nth-quadruple
; (Hint: funcții curry)
; În define-ul de mai jos nu am precizat parametrii funcției
; get-nth-tuple pentru ca voi înșivă să decideți care este
; modul optim în care funcția să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție,
; dar asistentul va observa dacă implementarea respectă cerința.
(define (get-nth-tuple n tuple fct f1 f2 f3)
  (foldl (lambda (f acc) (cond ((= f 1) (fct f1 acc))
                               ((= f 2) (fct f2 acc))
                               ((= f 3) (fct f3 acc))))
         tuple (get-transformations n))
  )


; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil
; (hint: aplicare parțială) o funcție care calculează al n-lea
; TPP din arbore, folosind transformările pe triplete.
(define (get-nth-ppt-from-matrix-transformations n)
  (get-nth-tuple n '(3 4 5) multiply T1 T2 T3)
  )


; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil 
; (hint: aplicare parțială) o funcție care calculează al n-lea 
; cvartet din arbore, folosind transformările pe cvartete.
(define (get-nth-quadruple n)
  (get-nth-tuple n '(1 1 2 3) apply Q1 Q2 Q3)
  )

; TODO
; Folosiți rezultatul întors de get-nth-quadruple pentru a 
; obține al n-lea TPP din arbore.

;quadruple care se transforma intr-un triplet
(define (Q4 g e f h) (list (* g h) (* 2 e f) (+ (* e e) (* f f))))

(define (get-nth-ppt-from-GH-quadruples n)
  (apply Q4 (get-nth-quadruple n))
  )
;; Pentru un asemenea cvartet (g, e, f, h), definim:
;;    a = gh,   b = 2ef,   c = e^2 + f^2