#lang info

(define collection "curly-fn")
(define scribblings '(["scribblings/curly-fn.scrbl"]))

(define deps
  '(["base" #:version "6.2"]
    "namespaced-transformer-lib"
    "rackunit-lib"))
(define build-deps
  '("racket-doc"
    "namespaced-transformer-doc"
    "scribble-lib"
    "scribble-code-examples"))
