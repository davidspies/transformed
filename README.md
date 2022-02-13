# transformed

Introduces a `newtype` wrapper for easy writing of new monad transformers. If your monad transformer
is built as a `newtype` wrapper around some combination of `mtl` transformers (excluding `ContT`),
you can derive the ability to pass instances of `mtl` class through using the `DerivingVia` strategy
with `Transformed`.

See [Example.hs](test/Example.hs) for an example.
