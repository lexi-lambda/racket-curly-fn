#lang info

(define collection "curly-fn")
(define scribblings '(["scribblings/curly-fn.scrbl"]))

(define deps
  '("base"
    "rackunit-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"
    "cover"))
