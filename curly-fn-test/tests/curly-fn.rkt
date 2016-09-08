#lang curly-fn racket/base

(require rackunit
         racket/list)

(test-case
 "Positional arguments"
 (check-equal? (#{list %3 %2 %1} 1 2 3) '(3 2 1))
 (check-equal? (#{cons % %1} 'x) '(x . x))
 (check-equal? (#{values %2} 'ignored 7) 7))

(test-case
 "Rest argument"
 (check-equal? (#{apply list %&} 1 2 3) '(1 2 3))
 (check-equal? (#{begin % %&} 1 2 3) '(2 3)))

(test-case
 "Keyword arguments"
 (check-equal? (#{values %:kw} #:kw 'x) 'x))

(test-case
 "Curry shorthand"
 (check-equal? (map #{+ 2} (range 1 4)) '(3 4 5)))

(test-case
 "Hygiene"
 (check-equal?
  (let ([lambda 5])
    (map #{+ % 1} '(1 2 3)))
  '(2 3 4)))

(module phase-levels racket/base
  (require (for-meta 1 racket/base)
           (for-meta 2 racket/base))
  (provide (for-meta 2 *-curried))
  (begin-for-syntax
    (begin-for-syntax
      (define *-curried #{*}))))
(require (for-meta -2 (submod "." phase-levels)))
(test-case
 "Usage at high phase levels"
 (check-equal? (*-curried 2 2) 4))
