; Sofía Margarita Hernández Muñoz
; A01655084
; Emiliano Saucedo Arriola
; A01659258
; Actividad 2.3 Programación funcional con Racket

#lang slideshow

; 10. Devuelve sub-listas que cuentan el número de veces que aparece un elemento
(define (encode lst)

  ; Variable contador
  (define num 0)

  (define (sub-cont lst num)
    (if (empty? lst)
        empty
        (if (= (length lst) 1)
            (list (append (list (+ 1 num)) lst))
            (if (equal? (first lst) (first (rest lst)))
                (sub-cont (rest lst) (+ 1 num))
                (append (list (append (list (+ 1 num)) (list (first lst)))) (sub-cont (rest lst) 0))))))
  ; Call
  (sub-cont lst num))

(encode '())
; ()
(encode '(a a a a b c c a a d e e e e))
; ((4 a) (1 b) (2 c) (2 a) (1 d) (4 e))
(encode '(1 2 3 4 5))
; ((1 1) (1 2) (1 3) (1 4) (1 5))
(encode '(9 9 9 9 9 9 9 9 9))
; ((9 9))
