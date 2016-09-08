#lang racket/base

(require racket/port
         syntax/readerr)

(provide make-extension-reader)

(define default-read-spec
  (lambda (in)
    (let ([spec (regexp-try-match #px"^[ \t]+(.*?)(?=\\s|$)" in)]) ;; if this changes, the regexp in planet's lang/reader.rkt must also change
      (and spec (let ([s (cadr spec)])
                  (if (equal? s "") #f s))))))

(define (lang-reader-module-paths str)
  (let* ([str (bytes->string/latin-1 str)]
         [sym (string->symbol str)])
    (and (module-path? sym)
         (vector `(submod ,sym reader)
                 (string->symbol (string-append str "/lang/reader"))))))

(define (make-extension-reader
         extension-name
         convert-read
         convert-read-syntax
         convert-get-info
         #:read-spec [read-spec default-read-spec])
    (define (get in export-sym src line col pos spec-as-stx? mk-fail-thunk)
      (define (bad str eof?)
        ((if eof? raise-read-eof-error raise-read-error)
         (let ([msg (format "bad language path following ~a" extension-name)])
           (if str (format "~a: ~a" msg str) msg))
         src line col pos
         (let-values ([(line col pos2) (port-next-location in)])
           (and pos pos2 (- pos2 pos)))))
      (let*-values ([(spec-line spec-col spec-pos) (port-next-location in)]
                    [(spec) (read-spec in)]
                    [(spec-end-line spec-end-col spec-end-pos) (port-next-location in)])
        (if (not spec)
            (bad #f (eof-object? (peek-byte in)))
            (let ([parsed-spec (lang-reader-module-paths spec)])
              (if parsed-spec
                  (let loop ([specs (if (vector? parsed-spec)
                                        (vector->list parsed-spec)
                                        (list parsed-spec))])
                    (define parsed-spec (car specs))
                    (define guarded-spec ((current-reader-guard) parsed-spec))
                    (if (or (null? (cdr specs))
                            (module-declared? guarded-spec #t))
                        (values
                         (dynamic-require guarded-spec export-sym
                                          (mk-fail-thunk spec))
                         (if spec-as-stx?
                             (datum->syntax #f
                                            guarded-spec
                                            (vector src spec-line spec-col spec-pos
                                                    (max 0 (- spec-end-pos spec-pos))))
                             guarded-spec))
                        (loop (cdr specs))))
                  (bad spec #f))))))

    (define (-get-info inp mod line col pos)
      (let-values ([(r next-mod)
                    (get inp 'get-info (object-name inp) line col pos #f
                         (lambda (spec)
                           (lambda ()
                             (lambda (inp mod line col pos)
                               (lambda (tag defval) defval)))))])
        (convert-get-info (r inp next-mod line col pos))))

    (define (read-fn in read-sym args src mod line col pos convert)
      (let ([get-info (-get-info (peeking-input-port in) mod line col pos)])
        (let-values ([(r next-mod)
                      (get in read-sym src #|mod|# line col pos
                           (eq? read-sym 'read-syntax)
                           (lambda (spec)
                             (lambda ()
                               (error read-sym "cannot find reader for `#lang ~a ~s'"
                                      extension-name
                                      spec))))])
          (let ([r (convert r get-info)])
            (if (and (procedure? r)
                     (procedure-arity-includes? r (+ 5 (length args))))
                (apply r (append args (list in next-mod line col pos)))
                (apply r (append args (list in))))))))

    (define (-read inp mod line col pos)
      (read-fn inp 'read null (object-name inp) mod line col pos
               convert-read))

    (define (-read-syntax src inp mod line col pos)
      (read-fn inp 'read-syntax (list src) src mod line col pos
               convert-read-syntax))

    (values -read -read-syntax -get-info))
