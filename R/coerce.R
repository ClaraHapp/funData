#' @include funDataClass.R 
NULL

#### Coercion to data.frame ####

#' Coerce functional data objects to a data.frame
#'
#' Coerce objects of class \code{funData},  \code{multiFunData} and
#' \code{irregFunData} to a data frame.
#'
#' @param x The functional data object that is to be transformed to a
#'   \code{data.frame}
#'
#' @return A data frame with columns \code{obs} (gives index of observed
#'   curve), \code{argvals1, ... argvalsd} with \code{d} the dimension of
#'   the support and \code{X} for the observed values. One-dimensional
#'   functions have only \code{argvals1}, two-dimensional functions
#'   (images) have \code{argvals1} and \code{argvals2}, etc.
#'  
#' @seealso \code{\linkS4class{funData}}, \code{\linkS4class{irregFunData}},
#'   \code{\linkS4class{multiFunData}}, \code{\link{data.frame}}
#'
#' @name as.data.frame.funData
#'  
#' @examples
#' # one-dimensional domain
#' f1 <- funData(argvals = 1:5, X = matrix(1:20, nrow = 4))
#' head(as.data.frame(f1))
#' 
#' # two-dimensional domain
#' f2 <- funData(argvals = list(1:5, 1:6), X = array(1:120, c(4,5,6)))
#' head(as.data.frame(f2))
#' 
#' # multivariate functional data
#' m1 <- multiFunData(f1, f2)
#' str(as.data.frame(m1))
#' 
#' # irregular functional data
#' i1 <- irregFunData(argvals = list(1:5, 2:4, 3:5), X = list(1:5, 2:4, -(3:1)))
#' head(as.data.frame(i1))
NULL

#' @keywords internal
setAs("funData", "data.frame", 
      def = function(from){
        
        d <- dimSupp(from)
        
        # expand all argument values
        allArgvals <- expand.grid(from@argvals, KEEP.OUT.ATTRS = TRUE)
        colnames(allArgvals) <- paste("argvals", 1:d, sep = "")
        row.names(allArgvals) <- NULL
        
        return(cbind(obs = rep(1:nObs(from), each = prod(nObsPoints(from))),
                     allArgvals,
                     X = as.numeric(aperm(from@X, c(1 + 1:d,1)))))
      })


#' @rdname as.data.frame.funData
#' @export as.data.frame
setMethod("as.data.frame", signature = "funData", 
          function(x){as(x, "data.frame")})

#' @keywords internal
setAs("multiFunData", "data.frame", 
      def = function(from){
        return(lapply(from, as.data.frame))
      })

#' @rdname as.data.frame.funData
#' @export as.data.frame
setMethod("as.data.frame", signature = "multiFunData", 
          function(x){as(x, "data.frame")})

#' @keywords internal
setAs("irregFunData", "data.frame", 
      def = function(from){
        
        return(data.frame(obs = rep(1:nObs(from), times = nObsPoints(from)),
                     argvals = unlist(from@argvals),
                     X = unlist(from@X)))
      })

#' @rdname as.data.frame.funData
#' @export as.data.frame
setMethod("as.data.frame", signature = "irregFunData", 
          function(x){as(x, "data.frame")})


#### Coercion to fd (from fda) ####

#' Convert a funData object to fd
#'
#' This function converts an object of class \code{\link{funData}} to an
#' object of class \code{\link[fda]{fd}} (from package \pkg{fda}). It
#' heavily builds on the function \code{\link[fda]{Data2fd}} from the
#' \pkg{fda} package. The \code{\link[fda]{fd}} representation assumes a
#' basis representation for the observed functions and therefore
#' implicitly smoothes the data. In \code{funData} objects, the data is
#' saved in 'raw' format.
#'
#' @section Warning: This function works only for funData objects on
#'   one-dimensional domains.
#'
#' @param object A \code{funData} object
#' @param ... Other parameters passed to \code{\link[fda]{Data2fd}}.
#'
#' @return An object of class \code{\link[fda]{fd}}.
#'
#' @export
#'
#' @seealso \code{\linkS4class{funData}}, \code{\link[fda]{fd}},
#'   \code{\link[fda]{Data2fd}}, \code{\link{fd2funData}}
#'
#' @examples
#' # Install / load package fda before running the examples
#' library("fda")
#'
#' # from Data2fd help
#' daybasis <- create.fourier.basis(c(0, 365), nbasis=65)
#' # funData object with temperature
#' tempFun <- funData(day.5, t(CanadianWeather$dailyAv[, , "Temperature.C"]))
#' # convert to fd
#' tempfd <- funData2fd(tempFun, daybasis)
#'
#' # plot to compare
#' par(mfrow = c(1,2))
#' plot(tempFun, main = "funData object (raw data)")
#' plot(tempfd, main = "fd object (smoothed)")
funData2fd <- function(object, ...) 
{
  if(!(requireNamespace("fda", quietly = TRUE)))
  {
    warning("Please install the fda package to use the funData2fd function for funData objects.")
    return()
  } 
  
  if(!inherits(object,"funData"))
    stop("Argument is not of class 'funData'.")
  
  if(dimSupp(object) > 1)
    stop("funData2fd is only defined for functions on one-dimensional domains.")
  
  return(fda::Data2fd(argvals = object@argvals[[1]], y = t(object@X), ...))
}


#' Convert an f2 object to funData
#'
#' This function converts an object of class \code{\link[fda]{fd}} (from
#' package \pkg{fda}) to an object of class \code{\link{funData}}. It
#' heavily builds on the function \code{\link[fda]{eval.fd}} from the
#' \pkg{fda} package. The \code{\link[fda]{fd}} representation assumes a
#' basis representation for the observed functions and therefore
#' implicitly smoothes the data. In \code{funData} objects, the data is
#' saved in 'raw' format.
#'
#' @section Warning: Time names in \code{fdobj$fdnames$time} are not
#'   preserved.
#'
#' @param fdobj An \code{fd} object
#' @param argvals A vector or a list of length one, containing a vector
#'   with argument values at which the functions in \code{fdobj} should be
#'   evaluated.
#' @param ... Other parameters passed to \code{\link[fda]{eval.fd}}.
#'
#' @return An object of class \code{\link{funData}}.
#'
#' @export
#'
#' @seealso \code{\linkS4class{funData}}, \code{\link[fda]{fd}},
#'   \code{\link[fda]{eval.fd}}
#'
#' @examples
#' # Install / load package fda before running the examples
#' library("fda")
#'
#' # from Data2fd help
#' daybasis <- create.fourier.basis(c(0, 365), nbasis=65)
#' # fd object of daily temperatures
#' tempfd <- Data2fd(argvals = day.5, y = CanadianWeather$dailyAv[,,"Temperature.C"], daybasis)
#' # convert to funData
#' tempFun <- fd2funData(tempfd, argvals = day.5)
#'
#' # plot to compare
#' par(mfrow = c(1,2))
#' plot(tempfd, main = "fd object")
#' plot(tempFun, main = "funData object") 
fd2funData <- function(fdobj, argvals, ...)
{
  if(!(requireNamespace("fda", quietly = TRUE)))
  {
    warning("Please install the fda package to use the fd2funData function for funData objects.")
    return()
  } 
  
  if(!is.numeric(argvals))
  {
    if(is.list(argvals) & length(argvals) == 1)
      argvals = unlist(argvals)
    else
      stop("Parameter 'argvals' must be either a vector of argument values or a list containing such a vector.")
  }
  
  # eval.fd checks validity of argvals
  return(funData(argvals = argvals, X = t(fda::eval.fd(argvals, fdobj, ...))))
}