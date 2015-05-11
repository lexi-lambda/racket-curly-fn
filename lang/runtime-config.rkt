#lang racket/base

(provide configure)

(require curly-fn)

(define (configure data)
  (define old-read (current-read-interaction))
  (define (new-read src in)
    (parameterize ([current-readtable (make-curly-fn-readtable)])
      (old-read src in)))
  (current-read-interaction new-read))