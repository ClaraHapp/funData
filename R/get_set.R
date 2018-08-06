#' Extract and set slots from functional data objects
#'
#' These functions can be used to extract and set the slots of
#' \code{funData}, \code{irregFunData} and \code{multiFunData} objects.
#'
#' Objects of class \code{funData} or \code{irregFunData} have two slots,
#' \code{argvals} (for the x-values) and \code{X} (for the y-values for
#' each observation). Using the \code{argvals} (alias: \code{argvals})
#' and \code{X} (alias: \code{X}) methods for the classes
#' \code{funData} and \code{irregFunData} is equivalent to accessing the
#' slots directly via \code{object@@argvals} and \code{object@@X}.
#' Analogously, the \code{argvals<-} and \code{X<-} functions are
#' equivalent to setting \code{object@@argvals} to \code{newArgvals} or
#' \code{object@@X} to \code{newX}, respectively. The new values must
#' hence have the same structure as the original ones. As an exception,
#' for an object of class \code{funData} the number of new observations in
#' \code{newX} may differ from the current (e.g. when adding new
#' observations). In this case, the function throws a warning.
#'
#' Objects of class \code{multiFunData} are lists of several
#' \code{funData} objects. The functions \code{argvals} and \code{X}
#' for \code{multiFunData} objects therefore return a list of the same
#' length as \code{object}, where each list element corresponds to the
#' \code{argvals} or \code{X} slot of the univariate element. The
#' \code{argvals<-} and \code{argvals} functions for
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
#' @export argvals
#'
#' @examples
#' ### Univariate
#' object <- funData(argvals = 1:5, X = rbind(1:5, 6:10))
#' object
#'
#' # get-methods
#' argvals(object)
#' X(object)
#'
#' # aliases
#' argvals(object)
#' X(object)
#'
#' # set-methods
#' argvals<-(object, 0:4)
#' object # no change
#' object <- argvals<-(object, 0:4) # reassign the result to object
#' object # now, argvals is changed
#' \dontrun{object <- argvals<-(object, 1:4)} # wrong length
#' object <- X<-(object, rbind(0:4, 5:9))
#' newObject <- X<-(object, rbind(0:4, 5:9, 10:14)) # warning: now 3 observations (was 2 before)
#' \dontrun{object <- X<-(object, rbind(1:4, 5:8))} # wrong length
#'
#' ### Univariate (irregular)
#' irregObject <- irregFunData(argvals = list(1:5, 2:4), X = list(2:6, 3:5))
#' irregObject
#'
#' # get-methods
#' argvals(irregObject)
#' X(irregObject)
#'
#' newIrregObject <- argvals<-(irregObject, list(0:4, 1:3))
#' newIrregObject <- X<-(irregObject, list(12:16, 13:15))
#'
#' ### Multivariate
#' multiObject <- multiFunData(object, funData(argvals = 1:3, X = rbind(3:5, 6:8)))
#' multiObject
#'
#' # get-methods
#' argvals(multiObject)
#' X(multiObject)
#'
#' # set-methods (for special cases see univariate version)
#' multiObject <- argvals<-(multiObject, list(5:1, 3:1))
#' multiObject <- X<-(multiObject, list(rbind(5:1, 10:6), rbind(5:3, 8:6)))
setGeneric("argvals", function(object) {standardGeneric("argvals")})

#' Get argvals slot for funData objects
#'
#' @seealso \code{\link{argvals}}
#'
#' @keywords internal
setMethod("argvals", signature = "funData",
          function(object){object@argvals})

#' Get argvals slot for multiFunData objects
#'
#' @seealso \code{\link{argvals}}
#'
#' @keywords internal
setMethod("argvals", signature = "multiFunData",
          function(object){lapply(object, argvals)})

#' Get argvals slot for irregular functional data objects
#'
#' @seealso \code{\link{argvals}}
#'
#' @keywords internal
setMethod("argvals", signature = "irregFunData",
          function(object){object@argvals})

### deprecated "getArgvals"
#' @rdname argvals
#' @export getArgvals
setGeneric("getArgvals", function(object) {standardGeneric("getArgvals")}, useAsDefault = argvals)

#' @rdname argvals
#' @keywords internal
setMethod("getArgvals", signature = "funData",
          function(object){
            .Deprecated("argvals")
            argvals(object)})

#' @rdname argvals
#' @keywords internal
setMethod("getArgvals", signature = "multiFunData",
          function(object){
            .Deprecated("argvals")
            argvals(object)})

#' @rdname argvals
#' @keywords internal
setMethod("getArgvals", signature = "irregFunData",
          function(object){
            .Deprecated("argvals")
            argvals(object)})

#### get X ####

#'@rdname argvals
#'
#'@export X
setGeneric("X", function(object) {standardGeneric("X")})

#' Get X slot for funData objects
#'
#' @seealso \code{\link{X}}
#'
#' @keywords internal
setMethod("X", signature = "funData",
          function(object){object@X})

#' Get X slot for multiFunData objects
#'
#' @seealso \code{\link{X}}
#'
#' @keywords internal
setMethod("X", signature = "multiFunData",
          function(object){sapply(object, X, simplify = FALSE)})

#' Get X slot for irregular functional data objects
#'
#' @seealso \code{\link{X}}
#'
#' @keywords internal
setMethod("X", signature = "irregFunData",
          function(object){object@X})


### deprecated: "getX"
#' @rdname argvals
#' @export getX
setGeneric("getX", function(object) {standardGeneric("getX")}, useAsDefault = X)

#' @rdname argvals
#' @keywords internal
setMethod("getX", signature = "funData",
          function(object){
            .Deprecated("X")
            X(object)})

#' @rdname argvals
#' @keywords internal
setMethod("getX", signature = "multiFunData",
          function(object){
            .Deprecated("X")
            X(object)})

#' @rdname argvals
#' @keywords internal
setMethod("getX", signature = "irregFunData",
          function(object){
            .Deprecated("X")
            X(object)})

#### set argvals ####

#' @rdname argvals
#'
#' @export argvals<-
setGeneric("argvals<-", function(object, value) {standardGeneric("argvals<-")})

#' Set argvals slot for funData objects
#'
#' @seealso \code{\link{argvals<-}}
#'
#' @keywords internal
setMethod("argvals<-", signature = "funData",
          function(object, value){
            if(is.numeric(value))
              value <- list(value)
            object@argvals <- value; validObject(object); return(object)
          })

#' Set argvals slot for multiFunData objects
#'
#' @seealso \code{\link{argvals<-}}
#'
#' @keywords internal
setMethod("argvals<-", signature = "multiFunData",
          function(object, value){
            if(length(object) != length(value))
              stop("multiFunData object and new argvals must have the same length")
            for(i in 1:length(object))
              argvals(object[[i]]) <- value[[i]]
            
            return(object)
          })


#' Set argvals slot for irregular functional objects
#'
#' @seealso \code{\link{argvals<-}}
#'
#' @keywords internal
setMethod("argvals<-", signature = "irregFunData",
          function(object, value){
            if(length(object@argvals) != length(value))
              stop("New argvals must be a list of the same length as the original argvals.")
            
            if(any(sapply(object@argvals, function(l){length(l)}) != sapply(value, function(l){length(l)})))
              stop("New argvals must have the same structure as the original argvals.")
            
            object@argvals <- value
            
            return(object)
          })


### alias argvals()<-
#' @rdname argvals
#' @export setArgvals
setGeneric("setArgvals", function(object, value) {standardGeneric("setArgvals")})

#' @rdname argvals
#' @keywords internal
setMethod("setArgvals", signature = "funData",
          function(object, value)
          {
            .Deprecated("argvals<-")
            argvals(object) <- value
            return(object)
          })

#' @rdname argvals
#' @keywords internal
setMethod("setArgvals", signature = "multiFunData",
          function(object, value)
          {
            .Deprecated("argvals<-")
            argvals(object) <- value
            return(object)
          })

#' @rdname argvals
#' @keywords internal
setMethod("setArgvals", signature = "irregFunData",
          function(object, value)
          {
            .Deprecated("argvals<-")
            argvals(object) <- value
            return(object)
          })

#### set X ####

#' @rdname argvals
#'
#' @export X<-
setGeneric("X<-", function(object, value) {standardGeneric("X<-")})

#' Set X slot for funData objects
#' @keywords internal
setMethod("X<-", signature = "funData",
          function(object, value){
            if(nrow(object@X) != nrow(value))
              warning("Number of observations has changed")
            object@X <- value; validObject(object); return(object)
          })

#' Set X slot for multiFunData objects
#' @keywords internal
setMethod("X<-", signature = "multiFunData",
          function(object, value){
            if(length(object) != length(value))
              stop("multiFunData object and new X must have the same length")
            
            if(diff(range(sapply(value, function(x){dim(x)[1]}))) != 0)
              stop("New X object must have the same number of observations in all elements!")
            
            multiFunData(mapply("X<-", object, value))
            
            return(object)
          })

#' Set X slot for irregular functional data objects
#' @keywords internal
setMethod("X<-", signature = "irregFunData",
          function(object, value){
            if(length(object@X) != length(value))
              stop("New X must be a list of the same length as the original X.")
            
            if(any(sapply(object@X, function(l){length(l)}) != sapply(value, function(l){length(l)})))
              stop("New X must have the same structure as the original X.")
            
            object@X <- value  
            
            return(object)
          })


### alias setX
#' @rdname argvals
#' @export setX
setGeneric("setX", function(object, value) {standardGeneric("setX")})

#' @rdname argvals
#' @keywords internal
setMethod("setX", signature = "funData",
          function(object, value)
          {
            .Deprecated("X<-")
            X(object) <- value
            return(object)
          })

#' @rdname argvals
#' @keywords internal
setMethod("setX", signature = "multiFunData",
          function(object, value)
          {
            .Deprecated("X<-")
            X(object) <- value
            return(object)
          })

#' @rdname argvals
#' @keywords internal
setMethod("setX", signature = "irregFunData",
          function(object, value)
          {
            .Deprecated("X<-")
            X(object) <- value
            return(object)
          })
