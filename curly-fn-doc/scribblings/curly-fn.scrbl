#lang scribble/manual

@(require scribble/bnf
          scribble-code-examples
          (for-label racket/base racket/list racket/function
                     curly-fn namespaced-transformer))

@(define (curly-fn-examples . stuff)
   (apply code-examples
          #:lang "curly-fn racket"
          #:show-lang-line @elem{@hash-lang[] @racketmodname[curly-fn] @racketmodname[racket]}
          #:context #'here
          stuff))

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

@curly-fn-examples|{
(#{list 1 % 3} 2)
(map #{- % 4} (range 9))
(#{apply list %&} 'a 'b 'c)
}|

As a special case, if the shorthand syntax is used, but no arguments prefixed with @litchar{%} are
included in the body, then the syntax becomes a shorthand for @racket[curry].

@curly-fn-examples|{
(map #{+ 2} (range 10))
}|

@section{Caveats}

The shorthand is expanded to @racket[lambda] by inspecting the datums within the function body. This
inspection step occurs @emph{before} subexpressions are expanded. This means that identifiers are
inspected as-is at read time, before any macros have a chance to manipulate the syntax. This is
probably a good thing, since it reduces the possibility of identifiers being lost or introduced via
syntax transformation, but it does mean it isn't possible to write a macro that expands to @racket[%]
or other identifiers and have those identifiers detected by the curly shorthand.

@section{Using the Curly Function Reader}

@defmodule[curly-fn #:link-target? #f]

@defproc[(make-curly-fn-readtable [#:readtable readtable readtable? (current-readtable)]) readtable?]{
                                                                                                      
Creates a readtable that extends @racket[readtable] to add support for reading literal function
shorthand syntax.

It isn’t possible to add this to an arbitrary readtable and have it produce lambdas, since lambdas
cannot be serialized to syntax objects. Instead, this readtable produces @tt{#%namespaced} datums,
which are expected to be bound to @racket[#%namespaced] from @racketmodname[namespaced-transformer].}
