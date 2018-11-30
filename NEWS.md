# xnet 0.1.4

## breaking changes

* `get_loo_fun()` : 

    - argument `homogenous` removed in favor of `x`. This
allows for extension of the function based on either an object or
the class of that object. 
    - `x` becomes the first argument.

## New features

* There's a new function `linear_filter` that fits a linear filter over
an adjacency matrix. This function comes with a class `linearFilter`.

## bug fixes and minor improvements

Currently none.
