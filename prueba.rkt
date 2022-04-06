; Sofía Margarita Hernández Muñoz
; A01655084
; Emiliano Saucedo Arriola
; A01659258
; Actividad 2.3 Programación funcional con Racket

#lang slideshow

; 10. Devuelve sub-listas que cuentan el número de veces que aparece un elemento
; (define (encode lst)

;   ; Variable contador
;   (define num 0)

;   (define (sub-cont lst num)
;     (if (empty? lst)
;         empty
;         (if (= (length lst) 1)
;             (list (append (list (+ 1 num)) lst))
;             (if (equal? (first lst) (first (rest lst)))
;                 (sub-cont (rest lst) (+ 1 num))
;                 (append (list (append (list (+ 1 num)) (list (first lst)))) (sub-cont (rest lst) 0))))))
;   ; Call
;   (sub-cont lst num))

; (define (encode lst)
;   (let ((ht (make-hash)))
;     (define (process key)
;       (hash-update! ht key (lambda (x) (+ x 1)) 0))
;     (for-each process lst)
;     (hash->list ht)))

; (define (encode lst)
;   (let ((ht (make-hash)))
;     (define (process key)
;       (hash-update! ht key (lambda (x) (+ x 1)) 0))
;     (for-each process lst)
;     (hash->list ht)))

(define (encode lst)
  (lst)
)

; (encode '())
; ; ()
; (encode '(a a a a b c c a a d e e e e))
; ; ((4 a) (1 b) (2 c) (2 a) (1 d) (4 e))
; (encode '(1 2 3 4 5))
; ; ((1 1) (1 2) (1 3) (1 4) (1 5))
; (encode '(9 9 9 9 9 9 9 9 9))
; ; ((9 9))

; 18. Calcula la integral utilizando la Regla de Simpson
(define (integral a b n f)

  ; Definimos h
  (define h (/ (- b a) n))

  ; Copiamos n
  (define ncopia n)

  (define (suma a b n f)
    (if (= n 0) ; Si es la primer suma / Caso base
        (f (+ a (* n h)))
        (if (= n ncopia) ; Si es la última suma / Recursivo
            (+ (f (+ a (* n h))) (suma a b (- n 1) f))
            (if (= (remainder n 2) 0) ; Si n es un número impar multiplicamos por 4, eoc. multiplicamos por 2 / Caso recursivo
                (+ (* (f (+ a (* n h))) 2) (suma a b (- n 1) f))
                (+ (* (f (+ a (* n h))) 4) (suma a b (- n 1) f))))))

  ; Call
  (* (/ h 3) (suma a b n f)))

(integral 0 1 10 (lambda (x) (* x x x)))
(integral 1 2 10
          (lambda (x)
            (integral 3 4 10
                      (lambda (y)
                        (* x y)))))