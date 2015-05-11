#lang racket/base

(require (for-syntax racket/base)
         syntax/module-reader
         curly-fn/private/curly-parser)

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
(define ((read-syntax* r) . args)
  (define read-syntax (read* r))
  (let* ([stx (apply read-syntax args)]
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
  (define contents
    ; disallow nested curly functions
    (parameterize ([current-readtable (make-curly-readtable (current-readtable)
                                                            handle-nesting-error)])
      (read-syntax/recursive src in #\{)))
  ; parse-curly-fn does all the heavy lifting
  (parse-curly-fn contents))

; error handler to trap nested curly function forms
(define (handle-nesting-error c in srcloc line-num col-num position)
  (raise-syntax-error 'curly-fn "nesting curly function forms is not permitted"
                      (read-syntax/recursive (object-name in) in #\{)))
