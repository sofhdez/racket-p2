#lang slideshow
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