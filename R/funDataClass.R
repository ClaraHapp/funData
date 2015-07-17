#' A class for (univariate) functional data
#' 
#' The \code{funData} class represents functional data on \eqn{d}{d}-dimensional
#' domains. The two slots represent the domain (x-values) and the values of the 
#' different observations (y-values).
#' 
#' Functional data can be seen as realizations of a random process \deqn{X: 
#' \mathcal{T} \to \mathrm{IR}}{X: \calT -> IR} on a \eqn{d}{d}-dimensional
#' domain \eqn{\mathcal{T}}{\calT}. The data is usually sampled on a fine grid
#' \eqn{T \subset \mathcal{T}}{T subset of \calT}, which is represented in the 
#' \code{xVal} slot of a \code{funData} object. All observations are assumed to 
#' be sampled over the same grid \eqn{T}{T}, but can contain missing values (see
#' below). If \eqn{\mathcal{T}}{\calT} is one-dimensional, \code{xVal} can be 
#' supplied either as a numeric vector, containing the x-values or as a list, 
#' containing such a vector. If \eqn{\mathcal{T}}{\calT} is higher-dimensional, 
#' \code{xVal} must always be supplied as a list, containing numeric vectors of 
#' the x-values in dimensions \eqn{1,\ldots,d}{1,\ldots,d}.
#' 
#' The observed values are represented in the \code{X} slot of a \code{funData} 
#' object, which is an array of dimension  \eqn{N \times M_1}{N x M_1} (for 
#' one-dimensional domains, or \eqn{N \times M_1 \times \ldots \times M_d}{N x 
#' M_1 x \ldots x M_d} for higher-dimensional domains). Here \eqn{N}{N} equals 
#' the number of observations and \eqn{M_1, \ldots, M_d}{M_1, \ldots, M_d} are 
#' the number of sampling points in dimension \eqn{1,\ldots, d}{1,\ldots, d}. 
#' Missing values in the observations are allowed and must be marked by 
#' \code{NA}.
#' 
#' Generic functions for the \code{funData} class include a print method, 
#' \link[=plot.funData]{plotting} and \link[=Arith.funData]{basic arithmetics}. 
#' Further methods for \code{funData}: \itemize{ \item \code{\link{dimSupp}}, 
#' \code{\link{nObs}}: Informations about the support dimensions and the number
#' of observations, \item \code{\link{getxVal}}, \code{\link{extractObs}}: 
#' Getting/Setting slot values (instead of accessing them directly via 
#' \code{funData@@xVal, funData@@X}) and extracting single observations or data
#' on a subset of the domain, \item \code{\link{integrate}}, \code{\link{norm}}:
#' Integrate all observations over their domain or calculating the
#' \eqn{L^2}{L^2} norm.}
#' 
#' A \code{funData} object can be coerced to a \code{multiFunData} object using 
#' \code{as.multiFunData(funDataObject).}
#' 
#' @slot xVal The domain \eqn{\mathcal{T}}{\calT} of the data. See Details.
#' @slot X The functional data samples. See Details.
#' 
#' @aliases funData 
#'   
#' @seealso \linkS4class{multiFunData}
#'   
#' @examples
#' ### Creating a one-dimensional funData object with 2 observations
#' # Basic
#' f1 <- new("funData", xVal = list(1:5), X = rbind(1:5,6:10))
#' # Using the constructor with first argument supplied as array
#' f2 <- funData(xVal = list(1:5), X = rbind(1:5, 6:10)) 
#' # Using the constructor with first argument supplied as numeric vector
#' f3 <- funData(xVal = 1:5, X = rbind(1:5, 6:10)) 
#' # Test if all the same
#' all.equal(f1,f2) 
#' all.equal(f1,f3)
#' # Display funData object in the console
#' f3 
#' 
#' # A more realistic object
#' xVal <- seq(0,2*pi,0.01)
#' object <- funData(xVal, outer(seq(0.75, 1.25, by = 0.05), sin(xVal)))
#' # Display gives basic information
#' object 
#' # Use the plot function to get an impression of the data
#' plot(object) 
#' 
#' 
#' ### Higher-dimensional funData objects with 2 observations
#' # Basic
#' g1 <- new("funData", xVal = list(1:5, 1:3),
#'                      X = array(1:30, dim = c(2,5,3))) 
#' # Using the constructor
#' g2 <- funData(xVal = list(1:5, 1:3),
#'               X = array(1:30, dim = c(2,5,3)))
#' # Test if the same
#' all.equal(g1,g2)
#' # Display funData object in the console
#' g2 
setClass("funData", representation = representation(xVal = "list", X = "array"))


# Validity checks for funData objects
setValidity("funData", function(object){
  if(!is(object@xVal, "list"))
  {
    return("xVal objects must be supplied as lists of numerics")
  } else {
    if(!all(sapply(object@xVal, is.numeric, simplify = TRUE)))
      return("all xVal elements must be numeric")
  }
  if(!is(object@X, "array"))
  {
    return("X elements must be supplied as arrays")
  } else {
    if(length(object@xVal) != length(dim(object@X)[-1]))
      return("xVal and X element have different support dimensions! X-Dimensions must be of the form N x M1 x ... x Md")
    
    if(!all(dim(object@X)[-1] == sapply(object@xVal, length, simplify = TRUE)))
      return("xVal and X have different number of sampling points! X-Dimensions must be of the form N x M1 x ... x Md")
  }
  
  return(TRUE)
})

#' Constructor for functional data objects, first argument (xVal) passed as list or vector of numerics
#' 
#' @param xVal A list of numeric vectors or a single numeric vector, giving the 
#'   sampling points in the domains. See Details.
#' @param X An array of dimension  \eqn{N \times M}{N x M} (for one-dimensional
#'   domains, or \eqn{N \times M_1 \times \ldots \times M_d}{N x M_1 x \ldots x
#'   M_d} for higher-dimensional domains), giving the observed values for
#'   \eqn{N}{N} individuals. Missing values can be included via \code{NA}. See
#'   Details.
#'   
#' @seealso \linkS4class{funData}
#'
#' @name funData-constructor
#' 
#' @docType methods
#' 
#' @export funData
#' 
#' @keywords internal
setGeneric("funData", function(xVal, X){standardGeneric("funData")})


#' @describeIn funData Constructor for functional data objects with \code{xVal} given as list.
#' 
#' @param xVal A list of numeric vectors or a single numeric vector, giving the 
#'   sampling points in the domains. See Details.
#' @param X An array of dimension  \eqn{N \times M}{N x M} (for one-dimensional
#'   domains, or \eqn{N \times M_1 \times \ldots \times M_d}{N x M_1 x \ldots x
#'   M_d} for higher-dimensional domains), giving the observed values for
#'   \eqn{N}{N} individuals. Missing values can be included via \code{NA}. See
#'   Details.
#' 
#' @docType methods
setMethod("funData", signature = c(xVal = "list", X = "array"),
          function(xVal, X){new("funData", xVal = xVal, X = X)})


#' @describeIn funData Constructor for functional data objects with \code{xVal}
#'   given as vector of numerics (only valid for one-dimensional domains).
#'   
#' @docType methods
setMethod("funData", signature = c(xVal = "numeric", X = "array"), 
          function(xVal, X){new("funData", xVal = list(xVal), X = X)})



#' A class for multivariate functional data
#' 
#' The \code{multiFunData} class represents multivariate functional data on 
#' (potentially) different domains, i.e. a multivariate functional data object 
#' is a vector of (univariate) functional data objects, just as a vector in 
#' \eqn{\mathrm{IR}^n}{IR^n} is a vector of \eqn{n}{n} scalars. In this implementation, a 
#' \code{multiFunData} object is represented as a list of univariate 
#' \code{funData} objects, see Details.
#' 
#' A \code{multiFunData} object is represented as a list of univariate 
#' \code{funData} objects, each having a \code{xVal} and \code{X} slot, 
#' representing the x-values and the observed y-values (see the 
#' \code{\link{funData}} class). When constructing a \code{multiFunData} object,
#' the  elements can be supplied as a list of \code{funData} objects or can be
#' passed directly as arguments to the constructor function.
#' 
#' Most functions implemented for the \code{\link{funData}} class are also 
#' implemented for \code{multiFunData} objects. In most cases, they simply apply
#' the corresponding univariate method to each element of the multivariate 
#' object and return it as a vector (if the result of the univariate function is
#' scalar, such as \code{\link{dimSupp}}) or as a \code{multiFunData} object (if
#' the result of the univariate function is a \code{funData} object, such as 
#' \code{\link{extractObs}}).
#' 
#' The norm of a multivariate functional data \eqn{f = (f_1 , \ldots, f_p)}{f =
#' (f_1 , \ldots, f_p)} is defined as \deqn{||| f ||| := \left(\sum_{j=1}^p ||
#' f_j ||^2 \right) ^{1/2}.}{||| f ||| := ( \sum || f_j ||^2 )^{1/2}.}
#' 
#' A \code{funData} object can be coerced to a \code{multiFunData} object with
#' one element using \code{as.multiFunData(funDataObject).}
#' 
#' @aliases multiFunData
#' 
#' @seealso \linkS4class{funData}
#'   
#' @examples
#' ### Creating a multifunData object with 2 observations on the same domain
#' # Univariate elements
#' x <- 1:5
#' f1 <- funData(x, rbind(x, x+1))
#' f2 <- funData(x,rbind(x^2, sin(x)))
#' # Basic
#' m1 <- new("multiFunData", list(f1,f2))
#' # Using the constructor, passing the elements as list
#' m2 <- multiFunData(list(f1,f2))
#' # Using the constructor, passing the elements directly
#' m3 <- multiFunData(f1,f2)
#' # Test if all the same
#' all.equal(m1,m2)
#' all.equal(m1,m3)
#' # Display multiFunData object in the console
#' m3
#' 
#' ### Creating a multifunData object with 2 observations on different domains (both 1D)
#' # A new element
#' y <- 1:3
#' g1 <- funData(y, rbind(3*y, y+4))
#' # Create the multiFunData object
#' m4 <- multiFunData(f1,g1)
#' # Display multiFunData object in the console
#' m4
#' 
#' ### Creating a multifunData object with 2 observations on different domains (1D and 2D)
#' # A new element
#' y <- 1:3; z <- 1:4
#' g2 <- funData(list(y,z), array(rnorm(24), dim = c(2,3,4)))
#' # Create the multiFunData object
#' m5 <- multiFunData(f1,g2)
#' # Display multiFunData object in the console
#' m5
#' 
#' ### A more realistic object
#' # element 1
#' x <- seq(0,2*pi, 0.01)
#' f1 <- funData(x, outer(seq(0.75, 1.25, length.out = 6), sin(x)))
#' # element 2
#' y <- seq(-1,1, 0.01); z <- seq(-0.5, 0.5, 0.01)
#' X2 <- array(NA, c(6, length(y), length(z)))
#' for(i in 1:6) X2[i,,] <- outer(y, z, function(x,y){sin(i*pi*y)*cos(i*pi*z)})
#' f2 <- funData(list(y,z), X2)
#' # MultiFunData Object
#' m6 <- multiFunData(f1,f2)
#' # Display multiFunData object in the console for basic information
#' m6
#' # Use the plot function to get an impression of the data
#' \dontrun{plot(m6)} # m6 has 2D element, must specify one observation for plotting
#' plot(m6, obs = 1, main = c("1st element (obs 1)", "2nd element (obs 1)"))
#' plot(m6, obs = 6, main = c("1st element (obs 6)", "2nd element (obs 6)"))
setClass("multiFunData", representation = "list")


# Validity check for multiFunData objects
setValidity("multiFunData", function(object){
  if(!is.list(object))
    return("Objects to multiFunData must be supplied as a list!")
  
  if(!all(sapply(object, is, "funData", simplify = TRUE)))
    return("elements of multiFunData must be of class funData!")
  
  if(diff(range(sapply(object,nObs)))!= 0)
    return("All elements must have the same number of observations!")
  
  return(TRUE)
})


#' Constructor for multivariate functional data objects
#' 
#' @seealso \linkS4class{multiFunData}
#' 
#' @name multiFunData-constructor
#' 
#' @docType methods
#' 
#' @export multiFunData
#' 
#' @keywords internal
setGeneric("multiFunData", function(...){standardGeneric("multiFunData")})


#' @describeIn multiFunData  Constructor for multivariate functional data
#'   objects.
#'   
#' @param ... A list of funData objects or several funData objects passed as one
#'   argument, each. See Details.
#'   
#' @docType methods
setMethod("multiFunData", signature = "ANY", 
          function(...){ 
            l <- list(...)
            if(!is.list(l[[1]]))
              new("multiFunData", l)
            else
              new("multiFunData", l[[1]])
          })



#' Coerce a funData object to class multiFunData
#' 
#' @seealso \linkS4class{funData}, \link{as.multiFunData}
#' 
#' @name funData-setAs
#' 
#' @keywords internal
setAs("funData", "multiFunData", 
      def = function(from){ new("multiFunData", list(from))})


 
#' Coerce a funData object to class multiFunData
#' 
#' Coerce a \code{funData} object to class \code{multiFunData}.
#' 
#' @param object The \code{funData} object that is to be converted to a
#'   \code{multiFunData} object of length 1.
#'   
#' @seealso \linkS4class{funData}, \linkS4class{multiFunData}
#'   
#' @export as.multiFunData
setGeneric("as.multiFunData", function(object){standardGeneric("as.multiFunData")})


#' @rdname as.multiFunData
setMethod("as.multiFunData", signature = "funData", 
          function(object){as(object, "multiFunData")})