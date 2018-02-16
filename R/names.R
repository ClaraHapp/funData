### Names for functional data objects
### no particular method needed for multiFunData!

#' @describeIn funData Get the names of the \code{funData} object.
#' 
#' @param x The \code{funData} object.
#' 
#' @docType methods
#'
#' @exportMethod names
setMethod("names", signature = "funData",
          function(x)
          {
           return(dimnames(x@X)[[1]] ) 
          })


#' @describeIn funData Set the names of the \code{funData} object.
#' 
#' @param x The \code{funData} object.
#' @param value The names to be given to the \code{funData} curves.
#' 
#' @docType methods
#'
#' @exportMethod names<-
setMethod("names<-", signature = "funData",
          function(x, value)
          {
            if(!is.null(value) & length(value) != nObs(x))
              stop("Names must have the same length as funData object.")
            
            dimnames(x@X)[[1]] <- value
            
            return(x)
          })


#' @describeIn irregFunData Get the names of the \code{irregFunData} object.
#' 
#' @param x The \code{irregFunData} object.
#' 
#' @docType methods
#'
#' @exportMethod names
setMethod("names", signature = "irregFunData",
          function(x)
          {
            return(names(x@argvals)) 
          })


#' @describeIn irregFunData Set the names of the \code{irregFunData} object.
#' 
#' @param x The \code{irregFunData} object.
#' @param value The names to be given to the \code{irregFunData} curves.
#' 
#' @docType methods
#'
#' @exportMethod names<-
setMethod("names<-", signature = "irregFunData",
          function(x, value)
          {
            if(!is.null(value) & length(value) != nObs(x))
              stop("Names must have the same length as funData object.")
            
            names(x@argvals) <- value
            names(x@X) <- value
            
            return(x)
          })