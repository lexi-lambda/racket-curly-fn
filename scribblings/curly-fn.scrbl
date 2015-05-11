#lang scribble/manual

@(require scribble/bnf
          (for-label racket/base racket/list racket/function
                     curly-fn))

@(define-syntax-rule (fn args ...)
   (elem (racketparenfont @tt{#})
         (racket {args ...})))

@(define >> @tt{>})

@title{Reader Function Literal Shorthand}

@section{Usage Overview}

@defmodule[curly-fn #:lang]

The @racketmodname[curly-fn] language is a meta-language that extends a language's readtable to
support function literals as reader syntax. This is inspired by Clojure's shorthand function literals
as well as @hyperlink["https://github.com/greghendershott/rackjure"]{Rackjure}'s implementation of
them in Racket.

The syntax for shorthand function literals is to simply prepend a @litchar{#} before @litchar|{{}}|,
and the contents will be read as a function literal. Arguments are denoted using identifiers prefixed
with @litchar{%}.

@itemlist[
 @item{@litchar{%} @kleeneplus{@nonterm{digit}} is a positional argument.}
 @item{@litchar{%} on its own is an alias for @litchar{%1}.}
 @item{@litchar{%&} is a rest argument.}
 @item{@litchar{%:} @nonterm{id} is a keyword argument.}]

@(racketmod
  curly-fn #,(racketmodname racket/base)
  #,>> (@#,fn[list 1 % 3] 2)
  #,(racketresult '(1 2 3))
  #,>> (map @#,fn[- % 4] (range 9))
  #,(racketresult '(-4 -3 -2 -1 0 1 2 3 4))
  #,>> (@#,fn[apply list %&] 'a 'b 'c)
  #,(racketresult '(a b c)))

As a special case, if the shorthand syntax is used, but no arguments prefixed with @litchar{%} are
included in the body, then the syntax becomes a shorthand for @racket[curry].

@(racketblock
  #,>> (map @#,fn[+ 2] (range 10))
  #,(racketresult '(2 3 4 5 6 7 8 9 10 11)))

@section{Caveats}

The shorthand is expanded to @racket[lambda] at read-time, not expansion-time. This means that
identifiers are inspected as-is at read time, before any macros have a chance to manipulate the
syntax.

Additionally, the shorthand syntax may not be nested. This is mostly to avoid complications when
considering the issue of argument identifier scope within nested functions, since those bindings are
introduced unhygienically by inspection of the datums at read-time.

@section{Using the Curly Function Reader}

@defmodule[curly-fn #:link-target? #f]

@defproc[(make-curly-fn-readtable [#:readtable readtable readtable? (current-readtable)]) readtable?]{
                                                                                                      
Creates a readtable that extends @racket[readtable] to add support for reading literal function
shorthand syntax.}
