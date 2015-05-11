#lang racket/base

(provide get-language-info)

(require racket/match)

(define (get-language-info old-prop)
  ; grab the get-language-info function from the “base” language for the meta-language
  (define main-lang-info
    (match old-prop
      [(vector mod-path mod-binding data)
       ((dynamic-require mod-path mod-binding) data)]
      [_ (λ (key default) default)]))
  (λ (key default)
    (case key
      ; register the custom runtime-config
      [(configure-runtime)
       (define config-vec #(curly-fn/lang/runtime-config configure #f))
       (define main-config (main-lang-info key default))
       ; the main lang's config might be #f
       (cons config-vec (or main-config '()))]
      [else (main-lang-info key default)])))