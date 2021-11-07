# funData 1.3-8

## New features
* Fixing DOI as required by Journal of Statistical Software.


# funData 1.3-7

## New features
* Fixing URLs as required by CRAN.


# funData 1.3-6

## New features
* Changing email address.

# funData 1.3-5

## New features
* Add links to vignette.

# funData 1.3-4

## New features
* Function `.intWeights` is not external any more, as used in MFPCA package.
* Update Author/Maintainer last name

# funData 1.3-3

## New features
* Suppress warnings in unit tests with fixed seed caused by changing the default RNG in latest R development version.


# funData 1.3-2

## New features
* Fixed typo in DESCRIPTION (testthat requires version >= 2.0.0)


# funData 1.3-1

## New features
* Consequent use of `seq_len(x)` instead of `1:x`.
* Use of type-safe `vapply` instead of `sapply`, where applicable.
* Standard `ggplot` function (package **ggplot**) as default for (deprecated) generic ggplot.


# funData 1.3

## New features
* New coercion methods `as.data.frame` and `funData2fd` / `fd2funData` (for `fd` objects from **fda** package).
* New functions `argvals` / `argvals<-` / `X` / `X<-` for accessing and setting slot values. The old functions  `getArgvals` / `setArgvals` / `getX` / `setX` are deprecated.
* New functions `autoplot` / `autolayer` for ggplot-type plotting (requires **ggplot2**, version 3.0.0). The old `ggplot` function is deprecated.
* Alias functions `subset` and `"["` for `extractObs` to achieve better compatibility with R's standard API.
* `print.summary` is now registered in `NAMESPACE`.
* Minor adjustments in output of `show` and `str` to fit into the standard 80 columns terminal.
* Documentation of `Arith` functions with more examples for standard operators
* Fixed some typos in the documentation.
* DESCRIPTION file now contains link to GitHub repo.

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
