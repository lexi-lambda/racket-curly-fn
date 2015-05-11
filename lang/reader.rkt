#lang racket/base

(require (for-syntax racket/base)
         syntax/module-reader
         curly-fn/private/curly-parser)

(provide make-curly-fn-readtable
         (rename-out [-read read]
                     [-read-syntax read-syntax]
                     [-get-info get-info]))

(define (make-curly-readtable base-table handler)
  (make-readtable
   base-table
   #\{ 'dispatch-macro
   handler))

(define (make-curly-fn-readtable #:readtable [readtable (current-readtable)])
  (make-curly-readtable readtable read-curly-contents))

(define ((read* r) . args)
  (parameterize ([current-readtable (make-curly-fn-readtable)])
    (apply r args)))

(define (str->spec str)
  (let* ([str (bytes->string/latin-1 str)]
         [sym (string->symbol str)])
    (if (module-path? sym)
        (vector `(submod ,sym reader)
                (string->symbol (string-append str "/lang/reader")))
        #f)))

(define-values (-read -read-syntax -get-info)
  (make-meta-reader
   'curly-fn "language path"
   str->spec read* read* values))

(define (read-curly-contents c in srcloc line-num col-num position)
  (define contents
    ; disallow nested curly functions
    (parameterize ([current-readtable (make-curly-readtable (current-readtable)
                                                            handle-nesting-error)])
      (read-syntax/recursive (object-name in) in #\{)))
  ; parse-curly-fn does all the heavy lifting
  (parse-curly-fn contents))

(define (handle-nesting-error c in srcloc line-num col-num position)
  (raise-syntax-error 'curly-fn "nesting curly function forms is not permitted"
                      (read-syntax/recursive (object-name in) in #\{)))
