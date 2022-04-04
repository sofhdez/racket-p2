; Sofía Margarita Hernández Muñoz
; A01655084
; Emiliano Saucedo Arriola
; A01659258
; Actividad 2.3 Programación funcional con Racket

#lang slideshow

; ---- Ejercicio 1 ---- FUNCIONA
(define (insert n lst)
    (cond
        [(null? lst) 
            (cons n '())]
        [(<= n (car lst))
            (cons n lst)]
        [(cons (car lst) (insert n (cdr lst)))])
)

(display "Ejercicio 1 - insert\n")
(insert 14 '())
(insert 4 '(5 6 7 8))
(insert 5 '(1 3 6 7 9 16))
(insert 10 '(1 5 6))

; ---- Ejercicio 2 ---- FUNC
(define (insertion-sort lst) 
    (cond
        [(null? lst) lst]
        [(insert(car lst)(insertion-sort(cdr lst)))]))

(display "\nEjercicio 2 - insertion-sort\n")
(insertion-sort '())
(insertion-sort '(4 3 6 8 3 0 9 1 7))
(insertion-sort '(1 2 3 4 5 6))
(insertion-sort '(5 5 5 1 5 5 5))

; ---- Ejercicio 3 ---- NO FUNC
(define (rotate-left n lst)
    (cond
        [(null? lst) '()]
        [(> n 0)
            (append(rotate-left(- n 1)(cdr lst))(cons (car lst) '()))
            ]
        [(= n 0) lst]))
    ; (if (null? LIST)
    ;     '()
    ;     (append (cdr LIST)
    ;             (cons (car LIST) '()))))

(display "\nEjercicio 3 - rotate-left\n")
(rotate-left 5 '())
(rotate-left 0 '(a b c d e f g))
(rotate-left 1 '(a b c d e f g))
; (rotate-left -1 '(a b c d e f g))
(rotate-left 2 '(a b c d e f g))
; (rotate-left -3 '(a b c d e f g))
(rotate-left 8 '(a b c d e f g))
; (rotate-left -8 '(a b c d e f g))
(rotate-left 45 '(a b c d e f g))
; (rotate-left -45 '(a b c d e f g))

; ---- Ejercicio 4 ---- 

; ---- Ejercicio 5 ----  FUNC
(define (gcd a b)
    (cond
        [(> a b) (gcd b (- a b))]
        [(< a b) (gcd a (- b a))]
        [else a]))
(display "\nEjercicio 5 - gcd\n")
(gcd 13 7919)
(gcd 20 16)
(gcd 54 24)
(gcd 6307 1995)
(gcd 48 180)
(gcd 42 56)

; ---- Ejercicio 6 ---- NO FUNC
(define (deep-reverse lst)
    (cond
        [(null? lst) '()]
        [
        (if (list? (first lst))
            (append(list(reverse(first lst)))(deep-reverse(rest lst)))
            (reverse(append(cons (first lst) '())(deep-reverse(rest lst)))))
            ]
        ))

    ; (cond
    ;     [(null? lst) '()]
    ;     [(append(deep-reverse(rest lst))(cons (first lst) '()))]
    ;     [(list? (first lst))
    ;         (append(list(reverse(first lst))) (deep-reverse(rest lst)))]
    ;     ))
(display "\nEjercicio 6 - deep-reverse\n")
(deep-reverse '())
(deep-reverse '(a (b c d) 3))
(deep-reverse '((1 2) 3 (4 (5 6))))
(deep-reverse '(a (b (c (d (e (f (g (h i j)))))))))

; ---- Ejercicio 7 ---- 
; ---- Ejercicio 8 ---- 
; ---- Ejercicio 9 ---- 
; ---- Ejercicio 10 ---- 
; ---- Ejercicio 11 ---- 
; ---- Ejercicio 12 ---- 
; ---- Ejercicio 13 ---- 
; ---- Ejercicio 14 ---- 
; ---- Ejercicio 15 ---- 
; ---- Ejercicio 16 ----
; ---- Ejercicio 17 ---- 
; ---- Ejercicio 18 ---- 

