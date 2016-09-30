#lang info

(define collection "curly-fn")
(define scribblings '(["scribblings/curly-fn.scrbl"]))

(define deps
  '(["base" #:version "6.1.1"]
    "rackunit-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"))
