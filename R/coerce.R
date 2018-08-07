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