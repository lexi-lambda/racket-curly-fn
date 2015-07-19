#lang racket/base

(require (for-meta -3 racket/base)
         (for-meta -2 racket/base)
         (for-meta -1 racket/base)
         (for-meta  1 racket/base)
         (for-meta  2 racket/base)
         (for-meta  3 racket/base)
         (for-meta  4 racket/base)
         racket/syntax
         syntax/module-reader
         curly-fn/private/curly-fn-transformer)

(provide make-curly-fn-readtable
         (rename-out [-read read]
                     [-read-syntax read-syntax]
                     [-get-info get-info]))

; builds a new readtable that dispatches on the { character
(define (make-curly-readtable base-table handler)
  (make-readtable
   base-table
   #\{ 'dispatch-macro
   handler))

; the user-facing function for creating curly-fn readtables
(define (make-curly-fn-readtable #:readtable [readtable (current-readtable)])
  (make-curly-readtable readtable read-curly-contents))

; a customized read function that installs the curly-fn readtable for its dynamic context
(define ((read* r) . args)
  (parameterize ([current-readtable (make-curly-fn-readtable)])
    (apply r args)))

; a customized read-syntax function to attach the 'module-language property to use the custom curly-fn
; language info
(define curly-fn-introducer (make-syntax-introducer #t))
(define ((read-syntax* r) . args)
  (define read-syntax (read* r))
  (let* ([stx (curly-fn-introducer (apply read-syntax args))]
         [old-prop (syntax-property stx 'module-language)]
         [new-prop `#(curly-fn/lang/language-info get-language-info ,old-prop)])
    (syntax-property stx 'module-language new-prop)))

; a helper function for use with make-meta-reader for extracting the main language's module path
(define (str->spec str)
  (let* ([str (bytes->string/latin-1 str)]
         [sym (string->symbol str)])
    (if (module-path? sym)
        (vector `(submod ,sym reader)
                (string->symbol (string-append str "/lang/reader")))
        #f)))

; create the meta-language reader functions
(define-values (-read -read-syntax -get-info)
  (make-meta-reader
   'curly-fn "language path"
   str->spec read* read-syntax* values))

; handler for actually performing the reading
(define (read-curly-contents c in src line-num col-num position)
  (with-syntax ([contents (curly-fn-introducer (read-syntax/recursive src in #\{))])
    (curly-fn-introducer
     #'(let ()
         (local-require curly-fn/private/curly-fn-transformer)
         (curly-fn contents)))))

; error handler to trap nested curly function forms
(define (handle-nesting-error c in srcloc line-num col-num position)
  (raise-syntax-error 'curly-fn "nesting curly function forms is not permitted"
                      (read-syntax/recursive (object-name in) in #\{)))
