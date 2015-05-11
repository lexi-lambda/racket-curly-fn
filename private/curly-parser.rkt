#lang racket/base

(require racket/list
         racket/match
         racket/syntax
         racket/function)

(provide parse-curly-fn)

; a struct containing information about the arguments provided to a curly-fn
(struct argument-info (max-positional has-rest? keywords))
(define argument-info-null (argument-info 0 #f null))

; reduces two argument information structs into a single value
(define/match (compose-argument-info a b)
  ([(argument-info m  r  kws)
    (argument-info m* r* kws*)]
   (argument-info (max m m*) (or r r*)
                  (remove-duplicates (append kws kws*)))))

; reduces a whole list of argument info structs
(define (reduce-argument-info . args)
  (foldl compose-argument-info argument-info-null args))
(define (reduce-argument-info* args)
  (apply reduce-argument-info args))

; parses the input, then assembles the proper output
(define (parse-curly-fn stx)
  ; all the actual parsing is done with find-arguments
  (match-define (argument-info max-positional has-rest? keywords) (find-arguments stx))
  (cond
    ; as long as at least one argument is provided, curly-fns are shorthand lambdas
    [(or (> max-positional 0) has-rest? (not (empty? keywords)))
     ; the positional args are based from 1, so these are generated from %1 to %n
     (define/with-syntax (positional-arg ...)
       (for/list ([i (in-range max-positional)])
         (format-id stx #:source stx "%~a" (add1 i))))
     ; the rest arg is simple and can be handled as the tail of the formals list
     (define/with-syntax rest-arg
       (if has-rest? (datum->syntax stx '%& stx) '()))
     ; keyword arguments need to be pairs of keywords and argument names
     (define/with-syntax (keyword-arg ...)
       (append* (for/list ([kw (in-list keywords)])
                  (list kw (format-id stx #:source stx "%:~a" (keyword->string kw))))))
     ; the body is just the original syntax passed in
     (define/with-syntax body stx)
     ; finally, % needs to be an alias for %1, which is handled with an inner (let ...) binding
     (define/with-syntax (aliases ...)
       (if (>= max-positional 1)
           `([,(datum->syntax stx '% stx)
              ,(datum->syntax stx '%1 stx)])
           '()))
     #'(lambda (positional-arg ... keyword-arg ... . rest-arg)
         (let (aliases ...) body))]
    ; otherwise, this is just a shorthand for curry
    [else
     (define/with-syntax (fn . body) stx)
     #'(let ()
         (local-require (only-in racket/function curry))
         ((curry fn) . body))]))

; locates usages of %, %n, %&, and %:kw
(define (find-arguments stx)
  (match (syntax-e stx)
    [(? symbol? s) (find-arguments/symbol s)]
    ; recurse through lists
    [(? list? l) (reduce-argument-info* (map find-arguments (syntax->list stx)))]
    [_ argument-info-null]))

; identifies the type of argument represented by a symbol
(define (find-arguments/symbol s)
  (match (symbol->string s)
    ; % is an alias for %1
    ["%" (argument-info 1 #f null)]
    ; %& is a rest argument
    ["%&" (argument-info 0 #t null)]
    ; %n is a by-position argument (based from 1)
    [(regexp #px"%(\\d+)" (list _ (app string->number n)))
     (argument-info n #f null)]
    ; %:kw is a keyword argument
    [(regexp #px"%:(.+)" (list _ (app string->keyword k)))
     (argument-info 0 #f (list k))]
    [_ argument-info-null]))
