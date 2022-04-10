; Sofía Margarita Hernández Muñoz
; A01655084
; Emiliano Saucedo Arriola
; A01659258
; Actividad 2.3 Programación funcional con Racket

#lang slideshow

; ---- Ejercicio 1 ----
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
; ⇒ (14)
(insert 4 '(5 6 7 8))
; ⇒ '(4 5 6 7 8)
(insert 5 '(1 3 6 7 9 16))
; ⇒ (1 3 5 6 7 9 16)
(insert 10 '(1 5 6))
; ⇒ (1 5 6 10)


; ---- Ejercicio 2 ----
(define (insertion-sort lst)
  (cond
    [(null? lst) lst]
    [(insert(car lst)(insertion-sort(cdr lst)))]))

(display "\nEjercicio 2 - insertion-sort\n")
(insertion-sort '())
; ⇒ ()
(insertion-sort '(4 3 6 8 3 0 9 1 7))
; ⇒ (0 1 3 3 4 6 7 8 9)
(insertion-sort '(1 2 3 4 5 6))
; ⇒ (1 2 3 4 5 6)
(insertion-sort '(5 5 5 1 5 5 5))
; ⇒ (1 5 5 5 5 5 5)

; ---- Ejercicio 3 ----
(define (rotate-left n lst)
  (cond
    [(empty? lst) '()]
    [(= n 0) lst]
    [(> n 0)(if( < n (length lst)) (append(list-tail lst n)(remv* (list-tail lst n) lst))
              (append(list-tail lst (remainder n (length lst)))(remv* (list-tail lst (remainder n (length lst))) lst)))]
    [(< n 0) (append(take-right lst (- (remainder n (length lst)) ))(drop-right lst (- (remainder n (length lst)))))]
    ))

(display "\nEjercicio 3 - rotate-left\n")
(rotate-left 5 '())
(rotate-left 0 '(a b c d e f g))
(rotate-left 1 '(a b c d e f g))
(rotate-left -1 '(a b c d e f g))
(rotate-left 3 '(a b c d e f g))
(rotate-left -3 '(a b c d e f g))
(rotate-left 8 '(a b c d e f g))
(rotate-left -8 '(a b c d e f g))
(rotate-left 45 '(a b c d e f g))
(rotate-left -45 '(a b c d e f g))

; ---- Ejercicio 4 ----  
(require math)

(define (prime-factors n)
  (append-map (lambda (x) (make-list (cadr x) (car x))) (factorize n)))

(display "\nEjercicio 4 - prime-factors\n")
(prime-factors 1)
(prime-factors 6)
(prime-factors 96)
(prime-factors 97)
(prime-factors 666)

; ---- Ejercicio 5 ----
(define (gcd a b)
  (cond
    [(> a b) (gcd b (- a b))]
    [(< a b) (gcd a (- b a))]
    [else a]))
(display "\nEjercicio 5 - gcd\n")
(gcd 13 7919)
; ⇒ 1
(gcd 20 16)
; ⇒ 4
(gcd 54 24)
; ⇒ 6
(gcd 6307 1995)
; ⇒ 7
(gcd 48 180)
; ⇒ 12
(gcd 42 56)
; ⇒ 14

; ---- Ejercicio 6 ----
(define (deep-reverse l)
  (if (list? l)
    (reverse (map deep-reverse l))l))

(display "\nEjercicio 6 - deep-reverse\n")
(deep-reverse '())
; ⇒ ()
(deep-reverse '(a (b c d) 3))
; ⇒ (3 (d c b) a)
(deep-reverse '((1 2) 3 (4 (5 6))))
; ⇒ (((6 5) 4) 3 (2 1))
(deep-reverse '(a (b (c (d (e (f (g (h i j)))))))))
; ⇒ ((((((((j i h) g) f) e) d) c) b) a)

; ---- Ejercicio 7 ----
(define (insert-at pos elmt lst)
  (if (empty? lst) (list elmt)
    (if (= 1 pos)
      (cons elmt lst)
      (cons (first lst)
        (insert-at (- pos 1) elmt (rest lst))))))

(define (insert-everywhere sym los)
  (map (lambda (i)
    (insert-at i sym los))
  (range 1 (+ 2 (length los)))))

(display "\nEjercicio 7 - insert-anywhere\n")
(insert-everywhere 1 '())
(insert-everywhere 1 '(a))
(insert-everywhere 1 '(a b c))
(insert-everywhere 1 '(a b c d e))
(insert-everywhere 'x '(1 2 3 4 5 6 7 8 9 10))

; ---- Ejercicio 8 ----
(define (put lst)
  (cond 
    [(equal? lst null) null]
    [(equal? (cdr lst) null)lst]
    [(equal? (car lst) (car (cdr lst)))
      (cons (car lst) (put (cdr lst)))]
    [true (list (car lst))]))

(define (throw lst)
  (cond 
    [(equal? lst null) null]
    [(equal? (cdr lst) null) null]
    [(equal? (car lst) (car (cdr lst)))
      (throw (cdr lst))]
    [true (cdr lst)]))

(define (pack lst)
  (if (equal? lst null)
      null
      (cons 
        (put lst) (pack (throw lst)))))

(display "\nEjercicio 8 - pack\n")
(pack '())
(pack '(a a a a b c c a a d e e e e))
(pack '(1 2 3 4 5))
(pack '(9 9 9 9 9 9 9 9 9))

; ---- Ejercicio 9 ----
(define (compress lst)
  (cond
    [(null? lst) null]
    ((null? (cdr lst)) lst)
    [(equal? (first lst) (first (rest lst)))
        (compress (rest lst))]
  [true (cons (first lst) (compress (rest lst)))]))


(display "\nEjercicio 9 - compress\n")
(compress '())
; ⇒ ()
(compress '(a b c d))
; ⇒ '(a b c d)
(compress '(a a a a b c c a a d e e e e))
; ⇒ (a b c a d)
(compress '(a a a a a a a a a a))

; ---- Ejercicio 10 ----
(define (encode lst)
  (map reverse (reverse (foldl (lambda (x y) 
    (if (or (empty? y) (not (equal? x (caar y))))
      (cons (list x 1) y)
      (cons (list x (add1 (cadar y))) (cdr y))))
    null lst))))

(display "\nEjercicio 10 - encode\n")
(encode '())

(encode '(a a a a b c c a a d e e e e))
(encode '(1 2 3 4 5))
(encode '(9 9 9 9 9 9 9 9 9))

; ---- Ejercicio 11 ----
(define (encode-modified lst)
  (define (encode-modified-aux lst)
    (cond 
          [(null? lst) null]
          [(= 1 (caar lst)) (cons (cadar lst) (encode-modified-aux (cdr lst)))]
          [else 
              (cons (car lst) (encode-modified-aux (cdr lst)))]))
  (encode-modified-aux (encode lst)))

(display "\nEjercicio 11 - there-exists-one?\n")
(encode-modified '())
(encode-modified '(a a a a b c c a a d e e e e))
(encode-modified '(1 2 3 4 5))
(encode-modified '(9 9 9 9 9 9 9 9 9))

; ---- Ejercicio 12 ----
(define (repeat-items num val)
  (if (= num 0)
      empty
      (cons val (repeat-items (- num 1) val))))

(define (my-decode xs)
  (define (my-decode-aux xs)
    (if (null? xs)
        empty
        (append (repeat-items (caar xs) (cadar xs))
                (my-decode-aux (cdr xs)))))
  (my-decode-aux xs))

(define (decode xs)
  (define (my-decode-aux xs)
    (cond [(null? xs) null]
          [(not (list? (car xs))) (cons 
                                      (cons 1 (cons (car xs) '()))
                                      (my-decode-aux (cdr xs)))]
          [else 
              (cons (car xs) (my-decode-aux (cdr xs)))]))
  
  (my-decode (my-decode-aux xs)))

(display "\nEjercicio 12 - decode\n")
(decode '())
(decode '((4 a) b (2 c) (2 a) d (4 e)))
(decode '(1 2 3 4 5))
(decode '((9 9)))

; ---- Ejercicio 13 ----
(define (args-swap f)
  (lambda (x y)
    (f y x)))

(display "\nEjercicio 13 - args-swap\n")
((args-swap list) 1 2)
((args-swap /) 8 2)
((args-swap cons) '(1 2 3) '(4 5 6))
((args-swap map) '(-1 1 2 5 10) /)

; ---- Ejercicio 14 ----
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
(there-exists-one? positive? '(-1 -10 4 -5 -2 -1))
(there-exists-one? negative? '(-1))
(there-exists-one? symbol? '(4 8 15 16 23 42))
(there-exists-one? symbol? '(4 8 15 sixteen 23 42))

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

; ---- Ejercicio 18 ----
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
