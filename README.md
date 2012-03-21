Yet Another Stupid Programming Language
=======================================

* Static type-checking
* Purity (or at least no mutation)
* Algebraic Data Types
* Type classes
* Better existential types

Numeric Context
---------------
Haskell always drives me batty with `fromIntegral` casts everywhere.
yaspl instead uses numeric contexts, where all math in a context is
presumed to be of that type.

    (int32 (+ 2147483647 2147483647))
    -2
    (num (+ 2147483647 2147483647))
    4294967294

yaspl also includes explicit casts:

    (float (->int32 (* 1.5 2.1)))
    3.0
