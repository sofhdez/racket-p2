#lang slideshow

(define (pack x)
  (cond
    [(null? x) '()]
    [(pair? x) (append (map (lambda (y) (if(equal? y (car x)) list y)) x))]))
  

(pack '())
(pack '(a a a a b c c a a d e e e e))
(pack '(1 2 3 4 5))
(pack '(9 9 9 9 9 9 9 9 9))
