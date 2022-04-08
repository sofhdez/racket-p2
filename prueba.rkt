; Sofía Margarita Hernández Muñoz
; A01655084
; Emiliano Saucedo Arriola
; A01659258
; Actividad 2.3 Programación funcional con Racket

#lang slideshow

; (define (pack lst)
;   (let ((hasht (make-hash)))
;     (define (process key)
;       (hash-update! hasht key (lambda (x) (+ x 1)) 0))
;     (for-each process lst)
;     (hash->list hasht)
;   )
; )

(define (pack lst)
  (foldl (lambda (key ht)
           (hash-update ht key add1 0))
         #hash() lst))
  
(pack '())
;⇒ ()
(pack '(a a a a b c c a a d e e e e))
;⇒ ((a a a a) (b) (c c) (a a) (d) (e e e e))
(pack '(1 2 3 4 5))
;⇒ ((1) (2) (3) (4) (5))
(pack '(9 9 9 9 9 9 9 9 9))
;⇒ ((9 9 9 9 9 9 9 9 9))