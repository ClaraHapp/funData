# funData 1.2

## New features
* Basic generics `names`, `names<-` `str` and `summary` for all functional data classes.
* S4 generic `norm` is now based on function `norm` in package base (no masking).
* More argument checking for user-facing functions including unit tests.
* Bug fixed for `meanFunction` and `irregFunData`.


# funData 1.1

## New features
* `ggplot` methods for all functional data classes.
* Simulation of univariate functional data on higher-dimensional domains.
* Math functions such as `log()` or `abs()` can be applied to all functional data classes.
* Coercion from `funData` to `irregFunData` class via `as.irregFunData` (useful e.g. for plotting).
* Scalar product method for multivariate functional data accepts weight vector.
* Unit test coverage is now close to 100%.
