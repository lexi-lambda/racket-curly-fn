#lang racket/base

(require racket/function
         syntax/parse/define
         (for-syntax (except-in racket/base
                                let let*)
                     racket/list
                     (rename-in racket/match
                                [match-let let]
                                [match-let* let*])
                     racket/syntax))

(provide curly-fn)

(begin-for-syntax
  ; a struct containing information about the arguments provided to a curly-fn
  (struct argument-info (max-positional has-rest? keywords) #:transparent)
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
  
  ; locates usages of %, %n, %&, and %:kw
  (define (find-arguments stx)
    (syntax-parse stx
      ; examine identifiers to see if they’re special
      [id:id (find-arguments/symbol (syntax-e #'id))]
      ; halt on nested curly-fn forms
      [(head:id . _) #:when (free-identifier=? #'head #'curly-fn) argument-info-null]
      ; recursively search through lists
      [(form ...) (reduce-argument-info* (map find-arguments (syntax->list stx)))]
      ; otherwise, return null
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
      [_ argument-info-null])))

(define-syntax-parser curly-fn
  [(_ terms)
   (let* ([terms-stx #'terms]
          [(argument-info max-positional has-rest? keywords)
           (find-arguments terms-stx)])
     (cond
       [(or (> max-positional 0) has-rest? (not (empty? keywords)))
        (with-syntax
            (; the positional args are based from 1, so these are generated from %1 to %n
             [(positional-arg ...)
              (for/list ([i (in-range max-positional)])
                (format-id terms-stx #:source terms-stx "%~a" (add1 i)))]
             ; the rest arg is simple and can be handled as the tail of the formals list
             [rest-arg (if has-rest? (datum->syntax terms-stx '%& terms-stx) '())]
             ; keyword arguments need to be pairs of keywords and argument names
             [(keyword-arg ...)
              (append* (for/list ([kw (in-list keywords)])
                         (list kw (format-id terms-stx #:source terms-stx
                                             "%:~a" (keyword->string kw)))))]
             ; the body is just the original syntax passed in
             [body terms-stx]
             ; finally, % needs to be an alias for %1, which is handled with an inner (let ...) block
             [(aliases ...)
              (if (>= max-positional 1)
                  `([,(datum->syntax terms-stx '% terms-stx)
                     ,(datum->syntax terms-stx '%1 terms-stx)])
                  '())])
          #'(lambda (positional-arg ... keyword-arg ... . rest-arg)
              (let (aliases ...) body)))]
       [else
        (with-syntax ([(fn . body) terms-stx])
          #'((curry fn) . body))]))])
