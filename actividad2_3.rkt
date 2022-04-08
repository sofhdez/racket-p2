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

; ---- Ejercicio 4 ----   FUNC
(require math)

(define (prime-factors n)
  ; car -> Returns the first element of the pair
  ; cdr -> Returns the second element of the pair
  ; card -> Returns (car (cdr x))
  ;   > (cadr '((1 2) 3 4)) -> 3
  (append-map (lambda (x) (make-list (cadr x) (car x))) (factorize n)))

(display "\nEjercicio 4 - prime-factors\n")
(prime-factors 1)
(prime-factors 6)
(prime-factors 96)
(prime-factors 97)
(prime-factors 666)

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

; ---- Ejercicio 6 ---- FUNC
(define (deep-reverse l)
  (if (list? l)
      (reverse (map deep-reverse l))
      l))

(display "\nEjercicio 6 - deep-reverse\n")
(deep-reverse '())
(deep-reverse '(a (b c d) 3))
(deep-reverse '((1 2) 3 (4 (5 6))))
(deep-reverse '(a (b (c (d (e (f (g (h i j)))))))))

; ---- Ejercicio 7 ----
(define (insert-at pos elmt lst)
  (if (empty? lst) (list elmt)
      (if (= 1 pos)
          (cons elmt lst)
          (cons (first lst)
                (insert-at (- pos 1) elmt (rest lst))))))

(define (insert-everywhere sym los)
  (map (lambda (i)
         (insert-at i sym los)
         )
       (range 1 (+ 2 (length los)))))

(display "\nEjercicio 7 - insert-anywhere\n")
(insert-everywhere 1 '())
(insert-everywhere 1 '(a))
(insert-everywhere 1 '(a b c))
(insert-everywhere 1 '(a b c d e))
(insert-everywhere 'x '(1 2 3 4 5 6 7 8 9 10))

; ---- Ejercicio 8 ----


; ---- Ejercicio 9 ----
(define (compress lst)
  (foldr (lambda (a b) (cons a (filter (lambda (c) (not (equal? a c))) b))) empty lst))

(display "\nEjercicio 9 - compress\n")
(compress '())
(compress '(a b c d))
(compress '(a a a a b c c a a d e e e e))
(compress '(a a a a a a a a a a))
; ---- Ejercicio 10 ----

; ---- Ejercicio 11 ----
; ---- Ejercicio 12 ----
; ---- Ejercicio 13 ----
(define (args-swap f)
  (λ (x y)
    (f y x)))
  
((args-swap list) 1 2)
; ⇒ (2 1)
((args-swap /) 8 2)
; ⇒ 1/4
((args-swap cons) '(1 2 3) '(4 5 6))
; ⇒ ((4 5 6) 1 2 3)
((args-swap map) '(-1 1 2 5 10) /)
; ⇒ (-1 1 1/2 1/5 1/10)

; ---- Ejercicio 14 ---- Funciona
(define (there-exists-one? pred lst)
  (cond
    [(empty? lst) #f]
    [else (cond
            [(pred (first lst))
             #t]
            [else map (there-exists-one? pred (rest lst))]
            )]
    )
  )

(display "\nEjercicio 14 - there-exists-one?\n")
(there-exists-one? positive? '())
; #f
(there-exists-one? positive? '(-1 -10 4 -5 -2 -1))
; #t
(there-exists-one? negative? '(-1))
; #t
(there-exists-one? symbol? '(4 8 15 16 23 42))
; #f
(there-exists-one? symbol? '(4 8 15 sixteen 23 42))
; #t

; ---- Ejercicio 15 ----
(define (linear-search lst x check)
  (cond
  [(null? lst) #f]
  [(if(check x (first lst)) 
    0
    (+ 1 (linear-search (rest lst) x check)))]))

(display "\nEjercicio 15 - linear-search\n")
(linear-search '() 5 =)
(linear-search '(48 77 30 31 5 20 91 92 69 97 28 32 17 18 96) 5 =)
(linear-search '("red" "blue" "green" "black" "white") "black" string=?)
(linear-search '(a b c d e f g h) 'h equal?)

; ---- Ejercicio 18 ---- Func
(define (integral a b n f)

  (define h (/ (- b a) n))

  (define ncopia n)

  (define (sumaIntegrales a b n f)
    ; Contador en 0
    (if (= n 0)
        (f (+ a (* n h)))

        ; En caso de que el contador llegue a la meta
        (if (= n ncopia)
            (+ (f (+ a (* n h))) (sumaIntegrales a b (- n 1) f))
            (if (= (remainder n 2) 0) ; Si n es un número impar multiplicamos por 4, eoc. multiplicamos por 2 / Caso recursivo
                ; Sino se multiplica por dos
                (+ (* (f (+ a (* n h))) 2) (sumaIntegrales a b (- n 1) f))
                ; Se multiplica por 4 si n es impar
                (+ (* (f (+ a (* n h))) 4) (sumaIntegrales a b (- n 1) f))
                )
            )
        )
    )

  ; Utilizamos las funciones ya definidas
  (* (/ h 3) (sumaIntegrales a b n f))
  )

(display "\nEjercicio 18 - integral\n")
(integral 0 1 10 (lambda (x) (* x x x)))
(integral 1 2 10
          (lambda (x)
            (integral 3 4 10
                      (lambda (y)
                        (* x y)))))