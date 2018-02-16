### Str methods for functional data objects


#' @describeIn funData A \code{str} method for \code{funData} objects, giving a compact overview of the structure.
#' 
#' @docType methods
#'
#' @exportMethod str
setMethod("str", signature = "funData",
          function(object, ...)
          {
            dims <- dimSupp(object)
            
            cat("Functional data object:\n", nObs(object), " observations of ",
                dims, "-dimensional support with ",
                paste(nObsPoints(object), collapse = " x "), " sampling points.\n", sep = "")
            
            cat("@argvals:\n")
            for(i in 1:dims)
            {
              cat("  [[",i, "]]: ", sep ="")
              str(object@argvals[[i]], ...)
            } 
            
            cat("\n@X:\n")
            str(object@X,...)
            
            invisible()
          })

#' @describeIn multiFunData A \code{str} method for \code{multiFunData} objects, giving a compact overview of the structure.
#' 
#' @docType methods
#'
#' @exportMethod str
setMethod("str", signature = "multiFunData",
          function(object, ...)
          {
            if (!is(object, "multiFunData")) 
              stop("str.multiFunData() called with non-multiFunData object.")
            
            cat("MultiFunData object with", length(object), "elements:\n")
            
            lapply(object, str.funData, ...)
            
            invisible()
          })


#' @describeIn irregFunData A \code{str} method for \code{irregFunData} objects, giving a compact overview of the structure.
#' 
#' @docType methods
#'
#' @exportMethod str
setMethod("str", signature = "irregFunData",
          function(object, ...)
          {
            dims <- dimSupp(object)
            
            # by default print only the first 10 entries
            dots <- list(...)
            if(is.null(dots$list.len))
              dots$list.len <- 10
            
            cat("IrregFunData object:\n", nObs(object), " observations of ",
                dims, "-dimensional support on " , length(unique(unlist(object@argvals))), " different x-values (",
                paste(range(nObsPoints(object)), collapse = " - "), " per curve).\n\n", sep = "")
            
            cat("@argvals: ")
            do.call("str", c(list(object = object@argvals), dots))
            
            cat("\n@X: ")
            do.call("str", c(list(object = object@X), dots))

            invisible()
          })