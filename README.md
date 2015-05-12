
# curly-fn [![Build Status](https://travis-ci.org/lexi-lambda/racket-curly-fn.svg?branch=master)](https://travis-ci.org/lexi-lambda/racket-curly-fn) [![Coverage Status](https://coveralls.io/repos/lexi-lambda/racket-curly-fn/badge.svg?branch=master)](https://coveralls.io/r/lexi-lambda/racket-curly-fn?branch=master)

A meta-language for adding Clojure-style shorthand function literals to arbitrary Racket languages.

```racket
> (#{/ % 2} 6)
3
```

Also supports a syntax for auto-currying, making partial application of functions much more concise.

```racket
> (map #{+ 2} (range 1 4))
'(3 4 5)
```

[See the documentation here (built nightly).](http://pkg-build.racket-lang.org/doc/curly-fn/index.html)
