#lang racket/base

(require (for-meta -3 racket/base)
         (for-meta -2 racket/base)
         (for-meta -1 racket/base)
         (for-meta  1 racket/base)
         (for-meta  2 racket/base)
         (for-meta  3 racket/base)
         (for-meta  4 racket/base)
         namespaced-transformer/info-key
         syntax/strip-context
         "../private/curly-fn-transformer.rkt"
         "../private/make-extension-reader.rkt")

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
(define (make-curly-fn-readtable #:readtable [readtable (current-readtable)]
                                 #:emit-namespaced? [emit-namespaced? #t])
  (make-curly-readtable readtable
                        (if emit-namespaced?
                            read-curly-contents/namespaced
                            read-curly-contents/introduced)))

; a customized read function that installs the curly-fn readtable for its dynamic context
(define ((read* r get-info) . args)
  (parameterize ([current-readtable (make-curly-fn-readtable
                                     #:emit-namespaced? (get-info key:supports-namespaced? #f))])
    (apply r args)))

; a customized read-syntax function to attach the 'module-language property to use the custom curly-fn
; language info
(define curly-fn-introducer (make-syntax-introducer #t))
(define ((read-syntax* r get-info) . args)
  (define read-syntax (read* r get-info))
  (let* ([stx (if (get-info key:supports-namespaced? #f)
                  (apply read-syntax args)
                  (curly-fn-introducer (apply read-syntax args)))]
         [old-prop (syntax-property stx 'module-language)]
         [new-prop `#(curly-fn/lang/language-info get-language-info ,old-prop)])
    (syntax-property stx 'module-language new-prop)))

; create the meta-language reader functions
(define-values (-read -read-syntax -get-info)
  (make-extension-reader
   'curly-fn read* read-syntax* values))

; handlers for actually performing the reading
(define (read-curly-contents/namespaced c in src line-num col-num position)
  (with-syntax ([contents (read-syntax/recursive src in #\{)])
    (strip-context
     #'(#%namespaced curly-fn/private/curly-fn-transformer
                     (curly-fn contents)))))

(define (read-curly-contents/introduced c in src line-num col-num position)
  (with-syntax ([contents (curly-fn-introducer (read-syntax/recursive src in #\{))])
    (curly-fn-introducer
     #'(let ()
         (local-require curly-fn/private/curly-fn-transformer)
         (curly-fn contents)))))
