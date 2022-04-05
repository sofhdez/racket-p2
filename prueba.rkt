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

; (encode '(a a a a b c c a a d e e e e))
; (encode '(1 2 3 4 5))
; (encode '(9 9 9 9 9 9 9 9 9))


; 14. Devuelve verdadero si hay un elemento en la lista que satisfaga la función f
(define (there-exists-one? pred lst)
  (if (empty? lst)
      #f  ; si no hay ninguno es falso
      (if (pred (first lst))
          #t  ; si hay alguno es verdadero

          ; va por todo el resto de la lista para verificar si existe alguno
          (there-exists-one? pred (rest lst)))))


(there-exists-one? positive? '())
(there-exists-one? positive? '(-1 -10 4 -5 -2 -1))
(there-exists-one? negative? '(-1))
(there-exists-one? symbol? '(4 8 15 16 23 42))
(there-exists-one? symbol? '(4 8 15 sixteen 23 42))