# funData

[![Travis-CI Build Status](https://travis-ci.org/ClaraHapp/funData.svg?branch=master)](https://travis-ci.org/ClaraHapp/funData) 
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/ClaraHapp/funData?branch=master&svg=true)](https://ci.appveyor.com/project/ClaraHapp/funData)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/funData)](https://CRAN.R-project.org/package=funData )
[![Coverage Status](https://img.shields.io/codecov/c/github/ClaraHapp/funData/master.svg)](https://codecov.io/github/ClaraHapp/funData?branch=master)



`funData` is an `R`-package that allows users to easily handle functional data. The data is stored in an object-oriented manner using S4 classes and methods.

## Highlights ##

`funData` provides a unified framework for different types of functional data:

* Univariate and multivariate data
* Regularly and irregularly sampled data
* Data with one-, two- (images) and even higher-dimensional domains

Important functionalities include: 

* Displaying, Plotting, including an interface to [`ggplot2`](https://CRAN.R-project.org/package=ggplot2)
* Subsetting (samples, domain)
* Basic arithmetics and averaging
* Integration over the domain
* A fully integrated simulation toolbox


## Installation ##

The `funData` pacakge is available on [`CRAN`](https://CRAN.R-project.org/package=funData).
To install the latest version directly from Github, please use `devtools::install_github("ClaraHapp/funData")` (install [`devtools`](https://github.com/hadley/devtools) before).

## Dependencies ##

The `funData` package depends on the `R`-packages [`fields`](https://CRAN.R-project.org/package=fields), [`foreach`](https://CRAN.R-project.org/package=foreach), [`abind`](https://CRAN.R-project.org/package=abind) and `methods`.

## References ##

The functionalities of the `funData` package and its interplay with the [`MFPCA`](https://CRAN.R-project.org/package=MFPCA) package for multivariate functional principal component analysis are described in:

C. Happ-Kurz (2020): [Object-Oriented Software for Functional Data.](http://doi.org/10.18637/jss.v093.i05>) *Journal of
Statistical Software*, 93(5): 1-38 .

## Bug reports ##

Please use [GitHub issues](https://github.com/ClaraHapp/funData/issues) for reporting bugs or issues.