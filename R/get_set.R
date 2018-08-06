#### get/set ####

#' Extract and set slots from functional data objects
#'
#' These functions can be used to extract and set the slots of
#' \code{funData}, \code{irregFunData} and \code{multiFunData} objects.
#'
#' Objects of class \code{funData} or \code{irregFunData} have two slots,
#' \code{argvals} (for the x-values) and \code{X} (for the y-values for
#' each observation). Using the \code{getArgvals} (alias: \code{argvals})
#' and \code{getX} (alias: \code{X}) methods for the classes
#' \code{funData} and \code{irregFunData} is equivalent to accessing the
#' slots directly via \code{object@@argvals} and \code{object@@X}.
#' Analogously, the \code{setArgvals} and \code{setX} functions are
#' equivalent to setting \code{object@@argvals} to \code{newArgvals} or
#' \code{object@@X} to \code{newX}, respectively. The new values must
#' hence have the same structure as the original ones. As an exception,
#' for an object of class \code{funData} the number of new observations in
#' \code{newX} may differ from the current (e.g. when adding new
#' observations). In this case, the function throws a warning.
#'
#' Objects of class \code{multiFunData} are lists of several
#' \code{funData} objects. The functions \code{getArgvals} and \code{getX}
#' for \code{multiFunData} objects therefore return a list of the same
#' length as \code{object}, where each list element corresponds to the
#' \code{argvals} or \code{X} slot of the univariate element. The
#' \code{setArgvals} and \code{getArgvals} functions for
#' \code{multiFunData} objects must be lists of the same length as
#' \code{object}, where each list element corresponds to the new
#' \code{argvals} or new \code{X} slot for the univariate elements.
#'
#' For all classes, the set functions do not change the object, unless
#' their result is assigned to \code{object} (see Examples).
#'
#' @param object An object of class \code{funData}, \code{irregFunData} or
#'   \code{multiFunData}.
#' @param newArgvals See Details.
#' @param newX See Details.
#'
#' @return See Details.
#'
#' @seealso \code{\linkS4class{funData}},
#'   \code{\linkS4class{irregFunData}}, \code{\linkS4class{multiFunData}}
#'
#' @export getArgvals
#'
#' @examples
#' ### Univariate
#' object <- funData(argvals = 1:5, X = rbind(1:5, 6:10))
#' object
#'
#' # get-methods
#' getArgvals(object)
#' getX(object)
#'
#' # aliases
#' argvals(object)
#' X(object)
#'
#' # set-methods
#' setArgvals(object, 0:4)
#' object # no change
#' object <- setArgvals(object, 0:4) # reassign the result to object
#' object # now, argvals is changed
#' \dontrun{object <- setArgvals(object, 1:4)} # wrong length
#' object <- setX(object, rbind(0:4, 5:9))
#' newObject <- setX(object, rbind(0:4, 5:9, 10:14)) # warning: now 3 observations (was 2 before)
#' \dontrun{object <- setX(object, rbind(1:4, 5:8))} # wrong length
#'
#' ### Univariate (irregular)
#' irregObject <- irregFunData(argvals = list(1:5, 2:4), X = list(2:6, 3:5))
#' irregObject
#'
#' # get-methods
#' getArgvals(irregObject)
#' getX(irregObject)
#'
#' newIrregObject <- setArgvals(irregObject, list(0:4, 1:3))
#' newIrregObject <- setX(irregObject, list(12:16, 13:15))
#'
#' ### Multivariate
#' multiObject <- multiFunData(object, funData(argvals = 1:3, X = rbind(3:5, 6:8)))
#' multiObject
#'
#' # get-methods
#' getArgvals(multiObject)
#' getX(multiObject)
#'
#' # set-methods (for special cases see univariate version)
#' multiObject <- setArgvals(multiObject, list(5:1, 3:1))
#' multiObject <- setX(multiObject, list(rbind(5:1, 10:6), rbind(5:3, 8:6)))
setGeneric("getArgvals", function(object) {standardGeneric("getArgvals")})

#' Get argvals slot for funData objects
#'
#' @seealso \code{\link{getArgvals}}
#'
#' @keywords internal
setMethod("getArgvals", signature = "funData",
          function(object){object@argvals})

#' Get argvals slot for multiFunData objects
#'
#' @seealso \code{\link{getArgvals}}
#'
#' @keywords internal
setMethod("getArgvals", signature = "multiFunData",
          function(object){lapply(object, getArgvals)})

#' Get argvals slot for irregular functional data objects
#'
#' @seealso \code{\link{getArgvals}}
#'
#' @keywords internal
setMethod("getArgvals", signature = "irregFunData",
          function(object){object@argvals})

### aliases
#' @rdname getArgvals
#' @export argvals
setGeneric("argvals", function(object) {standardGeneric("argvals")}, useAsDefault = getArgvals)

#' @rdname getArgvals
#' @keywords internal
setMethod("argvals", signature = "funData",
          function(object){getArgvals(object)})

#' @rdname getArgvals
#' @keywords internal
setMethod("argvals", signature = "multiFunData",
          function(object){getArgvals(object)})

#' @rdname getArgvals
#' @keywords internal
setMethod("argvals", signature = "irregFunData",
          function(object){getArgvals(object)})


#'@rdname getArgvals
#'
#'@export getX
setGeneric("getX", function(object) {standardGeneric("getX")})

#' Get X slot for funData objects
#'
#' @seealso \code{\link{getX}}
#'
#' @keywords internal
setMethod("getX", signature = "funData",
          function(object){object@X})

#' Get X slot for multiFunData objects
#'
#' @seealso \code{\link{getX}}
#'
#' @keywords internal
setMethod("getX", signature = "multiFunData",
          function(object){sapply(object, getX, simplify = FALSE)})

#' Get X slot for irregular functional data objects
#'
#' @seealso \code{\link{getX}}
#'
#' @keywords internal
setMethod("getX", signature = "irregFunData",
          function(object){object@X})


### aliases
#' @rdname getArgvals
#' @export X
setGeneric("X", function(object) {standardGeneric("X")}, useAsDefault = getX)

#' @rdname getArgvals
#' @keywords internal
setMethod("X", signature = "funData",
          function(object){getX(object)})

#' @rdname getArgvals
#' @keywords internal
setMethod("X", signature = "multiFunData",
          function(object){getX(object)})

#' @rdname getArgvals
#' @keywords internal
setMethod("X", signature = "irregFunData",
          function(object){getX(object)})


#' @rdname getArgvals
#'
#' @export setArgvals
setGeneric("setArgvals", function(object, newArgvals) {standardGeneric("setArgvals")})

#' Set argvals slot for funData objects
#'
#' @seealso \code{\link{setArgvals}}
#'
#' @keywords internal
setMethod("setArgvals", signature = "funData",
          function(object, newArgvals){
            if(is.numeric(newArgvals))
              newArgvals <- list(newArgvals)
            object@argvals <- newArgvals; validObject(object); return(object)
          })

#' Set argvals slot for multiFunData objects
#'
#' @seealso \code{\link{setArgvals}}
#'
#' @keywords internal
setMethod("setArgvals", signature = "multiFunData",
          function(object, newArgvals){
            if(length(object) != length(newArgvals))
              stop("multiFunData object and newArgvals must have the same length")
            multiFunData(mapply(setArgvals, object, newArgvals))
          })


#' Set argvals slot for irregular functional objects
#'
#' @seealso \code{\link{setArgvals}}
#'
#' @keywords internal
setMethod("setArgvals", signature = "irregFunData",
          function(object, newArgvals){
            if(length(object@argvals) != length(newArgvals))
              stop("newArgvals must be a list of the same length as the original argvals.")
            
            if(any(sapply(object@argvals, function(l){length(l)}) != sapply(newArgvals, function(l){length(l)})))
              stop("newArgvals must have the same structure as the original argvals.")
            
            object@argvals <- newArgvals
            
            return(object)
          })


### alias argvals()<-
#' @rdname getArgvals
#' @export argvals<-
setGeneric("argvals<-", function(object, value) {standardGeneric("argvals<-")})

#' @rdname getArgvals
#' @keywords internal
setMethod("argvals<-", signature = "funData",
          function(object, value)
          {
            # include checking procedures
            object@argvals <- setArgvals(object, value)@argvals
            return(object)
          })

#' @rdname getArgvals
#' @keywords internal
setMethod("argvals<-", signature = "multiFunData",
          function(object, value)
          {
            # include checking procedures
            tmp <- setArgvals(object, value)
            for(i in 1:length(object))
              object[[i]]@argvals <- tmp[[i]]@argvals
            
            return(object)
          })

#' @rdname getArgvals
#' @keywords internal
setMethod("argvals<-", signature = "irregFunData",
          function(object, value)
          {
            # include checking procedures
            object@argvals <- setArgvals(object, value)@argvals
            return(object)
          })



#' @rdname getArgvals
#'
#' @export setX
setGeneric("setX", function(object, newX) {standardGeneric("setX")})

#' Set X slot for funData objects
#'
#' @seealso \code{\link{setX}}
#'
#' @keywords internal
setMethod("setX", signature = "funData",
          function(object, newX){
            if(nrow(object@X) != nrow(newX))
              warning("setX: Number of observations has changed")
            object@X <- newX; validObject(object); return(object)
          })

#' Set X slot for multiFunData objects
#'
#' @seealso \code{\link{setX}}
#'
#' @keywords internal
setMethod("setX", signature = "multiFunData",
          function(object, newX){
            if(length(object) != length(newX))
              stop("multiFunData object and newX must have the same length")
            
            if(diff(range(sapply(newX, function(x){dim(x)[1]}))) != 0)
              stop("newX object must have the same number of observations in all elements!")
            
            multiFunData(mapply(setX, object, newX))
          })

#' Set X slot for irregular functional data objects
#'
#' @seealso \code{\link{setX}}
#'
#' @keywords internal
setMethod("setX", signature = "irregFunData",
          function(object, newX){
            if(length(object@X) != length(newX))
              stop("newX must be a list of the same length as the original X.")
            
            if(any(sapply(object@X, function(l){length(l)}) != sapply(newX, function(l){length(l)})))
              stop("newX must have the same structure as the original X.")
            
            object@X <- newX  
            
            return(object)
          })


### alias X()<-
#' @rdname getArgvals
#' @export X<-
setGeneric("X<-", function(object, value) {standardGeneric("X<-")})

#' @rdname getArgvals
#' @keywords internal
setMethod("X<-", signature = "funData",
          function(object, value)
          {
            # include checking procedures
            object@X <- setX(object, value)@X
            return(object)
          })

#' @rdname getArgvals
#' @keywords internal
setMethod("X<-", signature = "multiFunData",
          function(object, value)
          {
            # include checking procedures
            tmp <- setX(object, value)
            for(i in 1:length(object))
              object[[i]]@X <- tmp[[i]]@X
            
            return(object)
          })

#' @rdname getArgvals
#' @keywords internal
setMethod("X<-", signature = "irregFunData",
          function(object, value)
          {
            # include checking procedures
            object@X <- setX(object, value)@X
            return(object)
          })
