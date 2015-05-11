
# curly-fn

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
