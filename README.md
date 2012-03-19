Yet Another Stupid Programming Language
---------------------------------------

* Static type-checking
* Purity
* Type classes
* Better existential types
* Maybe S-expressions
* Some other stuff

Numeric Context
---------------
Haskell always drives me batty with `fromIntegral` casts everywhere.
yaspl instead uses numeric contexts, where all math in a context is
presumed to be of that type. Values are unsafely cast when they're
returned to the calling context:

    (int32 (+ 2147483647 2147483647))
    -2
    (num (+ 2147483647 2147483647))
    4294967294
