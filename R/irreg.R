library(funData)


setClass("irregFunData", representation = representation(xVal = "list", X = "list"))


# Validity checks for funData objects
setValidity("irregFunData", function(object){
  if(!is(object@xVal, "list"))
    return("xVal objects must be supplied as lists")
  
  if(!is(object@X, "list"))
    return("X elements must be supplied as lists")
  
  if(length(object@xVal) != length(object@X))
    return("Different number of observations for xVal and X")
  
  if(any(mapply(function(x,y){dim(as.array(x)) != dim(as.array(y))}, object@xVal, object@X)))
    return("xVal and X have different numbers of observation points")
  
  return(TRUE)
})

setGeneric("irregFunData", function(xVal, X){standardGeneric("irregFunData")})

setMethod("irregFunData", signature = c(xVal = "list", X = "list"),
          function(xVal, X){new("irregFunData", xVal = xVal, X = X)})


setMethod("dimSupp", signature = "irregFunData",
          function(object){length(dim(as.array(object@xVal[[1]])))})

setMethod("nObs", signature = "irregFunData",
          function(object){length(object@X)})


setMethod("extractObs", signature = signature("irregFunData", "ANY", "ANY"),
          function(object, obs, xVal){
            
            if(dimSupp(object) > 1)
              stop("extracting observations is not implemented yet for functional data of dimension > 1")
            
            if(!is.numeric(obs))
              stop("Supply observations as numeric vector")
            
            if(!all((1:nObs(object))[obs] %in% 1:nObs(object)))
              stop("Trying to extract observations that do not exist!")
            
            if(!is.list(xVal))
            {
              if(is.numeric(xVal))
                xVal = list(xVal)
              else
                stop("Supply xVals for exstracted observations either as list or as numeric vector")
            }
            
            if(!any(unlist(xVal) %in% unlist(object@xVal[obs])))
              stop("Trying to extract x-values that do not exist!")
            
            
            extractxVal <- extractX <- vector("list", length(obs))
            omit <- NULL
            
            for(i in 1:length(obs))
            {
              ind <- which(object@xVal[[obs[i]]] %in% unlist(xVal))
              
              if(length(ind) == 0)
              {
                warning("Some functions were not observed on the given xVal and therefore removed.")
                
                omit <- c(omit, i)
              }
              
              extractxVal[[i]] <- object@xVal[[obs[i]]][ind]
              extractX[[i]] <- object@X[[obs[i]]][ind]
            }
            
            # omit empty observations
            extractxVal[omit] <- NULL
            extractX[omit] <- NULL
            
            return(irregFunData(extractxVal, extractX))
          })


setMethod("getxVal", signature = "irregFunData",
          function(object){object@xVal})

setMethod("getX", signature = "irregFunData",
          function(object){object@X})

setGeneric("nObsPoints", function(object) {standardGeneric("nObsPoints")})

setMethod("nObsPoints", signature = "irregFunData", 
          function(object){sapply(object@xVal, function(l){length(l)})})




setAs("irregFunData", "funData", 
      def = function(from){ 
        
        if(dimSupp(from) > 1)
          stop("as.funData is implemented only for irregular functional data on one-dimensional domains.")
        
        xVal <- sort(unique(unlist(from@xVal)))
        
        X <- array(NA, dim = c(nObs(from), length(xVal)))
        
        for(i in 1:nObs(from))
          X[i, xVal %in% from@xVal[[i]]] <- from@X[[i]]
        
        return(funData(xVal = xVal, X = X))})



# Coerce a funData object to class multiFunData
# 
# Coerce a \code{funData} object to class \code{multiFunData}.
# 
# @param object The \code{funData} object that is to be converted to a
#   \code{multiFunData} object of length 1.
#   
# @seealso \linkS4class{funData}, \linkS4class{multiFunData}
#   
# @export as.multiFunData
setGeneric("as.funData", function(object){standardGeneric("as.funData")})


# @rdname as.multiFunData
setMethod("as.funData", signature = "irregFunData", 
          function(object){as(object, "funData")})

print.irregFunData <- function(x,...){
  cat("Irregular functional data object with", nObs(x) ,"observations of", dimSupp(x) ,"- dimensional support\n")
  
  cat("xVal:\n\tValues in ", paste(round(range(x@xVal),3), collapse = " ... "), ".\n", sep = "")
  
  cat("X:\n\tValues in ", paste(round(range(x@X),3), collapse = " ... "),".\n", sep = "")
  
  cat("Total:\n\t", length(unlist(f@xVal)), " observations on " , length(unique(unlist(f@xVal))), " different x-values (",
      paste(range(nObsPoints(x)), collapse = " - "), " per observation).\n", sep = "")
}

setMethod("show", signature = "irregFunData",
          function(object){print.irregFunData(object)})


plot.irregFunData <- function(x, y, obs = 1:nObs(x), type = "p", pch = 20,
                              col =rainbow(nObs(x)), xlab = "xVal", ylab = "",
                              add = FALSE, ...)
{
  
  if(add == FALSE) # plot new window
  {
    plot(x = NULL, y = NULL, type = "n", xlim = range(x@xVal[obs]), ylim = range(x@X[obs]), xlab= xlab, ylab = ylab, ...)
  }
  
  for(i in obs)
    points(x = x@xVal[[i]], y = x@X[[i]], type = type, pch = pch, col = col[i], ...)
  
}

# @rdname plot.funData
# 
# @exportMethod plot
setMethod("plot", signature = signature(x = "irregFunData", y = "missing"),
          function(x,y,...){plot.irregFunData(x,y,...)})


plot.xVals <- function(x, y, type = "p", pch = 20,
                       col =rainbow(nObs(x)), xlab = "xVal", ylab = "",
                       add = FALSE, ...)
{
  
  plot(x = NULL, y = NULL, type = "n", xlim = range(x@xVal), ylim = c(1,nObs(x)), xlab= xlab, ylab = ylab, ...)
  
  
  for(i in 1:nObs(x))
    points(x = x@xVal[[i]], y = rep(i, length(x@xVal[[i]])), type = type, pch = pch, col = col[i], ...)
  
}


# @rdname Arith.funData
setMethod("Arith", signature = c(e1 = "irregFunData", e2 = "numeric"),
          function(e1, e2) {
            f <- function(x,y){methods::callGeneric(x,y)} # helper function (callGeneric not applicable in lapply)
            irregFunData(xVal = e1@xVal, X = lapply(e1@X, function(x){f(x,e2)}))
          })

# @rdname Arith.funData
setMethod("Arith", signature = c(e1 = "numeric", e2 = "irregFunData"),
          function(e1, e2) {
            f <- function(x,y){methods::callGeneric(x,y)} # helper function (callGeneric not applicable in lapply)
            irregFunData(xVal = e2@xVal, X = lapply(e2@X, function(x){f(e1,x)}))
          })



# @rdname Arith.funData
setMethod("Arith", signature = c(e1 = "irregFunData", e2 = "funData"),
          function(e1, e2){
            if(any(c(dimSupp(e1), dimSupp(e2)) != 1))
              stop("Arithmetic operations: defined only for irregFunData objects with one-dimensional domain")
            
            if(!any(unlist(e1@xVal) %in% e2@xVal[[1]]))
              stop("arithmetic operations: irregFunData object must be defined on a subdomain of funData object!")
            
            # if funData object has only a single observation: apply to all of the other object
            if(nObs(e1) != nObs(e2))
            {
              if(nObs(e2) == 1 & nObs(e1) > 1) 
                e2@X <- t(replicate(nObs(e1),e2@X[1,]))            
              else
                stop("Arithmetic operations: funData and irregFunData objects must have same number of observations (or nObs(funDataObject)=1)")
            }
            f <- function(x,y){methods::callGeneric(x,y)} # helper function (callGeneric not applicable in lapply)
            irregFunData(xVal = e1@xVal, X = sapply(1:nObs(e1), function(i){f(e1@X[[i]], e2@X[i,e2@xVal[[1]] %in% e1@xVal[[i]]])}, simplify = FALSE))
          })


setMethod("Arith", signature = c(e1 = "funData", e2 = "irregFunData"),
          function(e1, e2){
            if(any(c(dimSupp(e2), dimSupp(e1)) != 1))
              stop("Arithmetic operations: defined only for irregFunData objects with one-dimensional domain")
            
            if(!any(unlist(e2@xVal) %in% e1@xVal[[1]]))
              stop("arithmetic operations: irregFunData object must be defined on a subdomain of funData object!")
            
            # if funData object has only a single observation: apply to all of the other object
            if(nObs(e2) != nObs(e1))
            {
              if(nObs(e1) == 1 & nObs(e2) > 1) 
                e1@X <- t(replicate(nObs(e2),e1@X[1,]))            
              else
                stop("Arithmetic operations: funData and irregFunData objects must have same number of observations (or nObs(funDataObject)=1)")
            }
            f <- function(x,y){methods::callGeneric(x,y)} # helper function (callGeneric not applicable in lapply)
            irregFunData(xVal = e2@xVal, X = sapply(1:nObs(e2), function(i){f(e2@X[[i]], e1@X[i,e1@xVal[[1]] %in% e2@xVal[[i]]])}, simplify = FALSE))
          })