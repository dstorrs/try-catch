#lang info

(define collection "try-catch")
(define version "0.1")
(define deps '("base"
               "syntax-classes-lib"
               "test-more"
               ))
(define scribblings '(("scribblings/try-catch.scrbl" ())))

(define test-omit-paths '())
(define build-deps '("racket-doc"
                     "scribble-lib"))
