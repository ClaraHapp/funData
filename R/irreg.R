# library(funData)

#  is defined only on one-dimensional domains yet
setClass("irregFunData", representation = representation(xVal = "list", X = "list"))


# Validity checks for irregfunData objects
setValidity("irregFunData", function(object){
  if(!is(object@xVal, "list"))
    return("xVal objects must be supplied as list (of numerics)")
  
  if(any(sapply(object@xVal, function(l){!is.numeric(l)})))
    return("xVal must be supplied as list of numerics")
  
  if(!is(object@X, "list"))
    return("X elements must be supplied as list (of numerics)")
  
  if(any(sapply(object@X, function(l){!is.numeric(l)})))
    return("X must be supplied as list of numerics")
  
  if(length(object@xVal) != length(object@X))
    return("Different number of observations for xVal and X")
  
  if(any(mapply(function(x,y){dim(as.array(x)) != dim(as.array(y))}, object@xVal, object@X)))
    return("Different numbers of observation points in xVal and X")
  
  return(TRUE)
})

setGeneric("irregFunData", function(xVal, X){standardGeneric("irregFunData")})

setMethod("irregFunData", signature = c(xVal = "list", X = "list"),
          function(xVal, X){new("irregFunData", xVal = xVal, X = X)})




setMethod("nObs", signature = "irregFunData",
          function(object){length(object@X)})











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



# Coerce an irregFunData object to class funData
setGeneric("as.funData", function(object){standardGeneric("as.funData")})



setMethod("as.funData", signature = "irregFunData", 
          function(object){as(object, "funData")})

print.irregFunData <- function(x,...){
  cat("Irregular functional data object with", nObs(x) ,"observations of", dimSupp(x) ,"- dimensional support\n")
  
  cat("xVal:\n\tValues in ", paste(round(range(x@xVal),3), collapse = " ... "), ".\n", sep = "")
  
  cat("X:\n\tValues in ", paste(round(range(x@X),3), collapse = " ... "),".\n", sep = "")
  
  cat("Total:\n\t", length(unlist(x@xVal)), " observations on " , length(unique(unlist(x@xVal))), " different x-values (",
      paste(range(nObsPoints(x)), collapse = " - "), " per observation).\n", sep = "")
}

setMethod("show", signature = "irregFunData",
          function(object){print.irregFunData(object)})


plot.irregFunData <- function(x, y, obs = 1:nObs(x), type = "b", pch = 20,
                              col =rainbow(nObs(x)), xlab = "xVal", ylab = "",
                              xlim = range(x@xVal[obs]), ylim = range(x@X[obs]),
                              add = FALSE, ...)
{
  if(length(col) < nObs(x))
    col <- rep(col, nObs(x))
  
  if(add == FALSE) # plot new window
  {
    plot(x = NULL, y = NULL, type = "n", xlim = xlim, ylim = ylim,  xlab= xlab, ylab = ylab, ...)
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
                       add = FALSE, xlim = range(x@xVal), ylim = c(1,nObs(x)), ...)
{
  
  plot(x = NULL, y = NULL, type = "n", xlim = xlim, ylim = ylim, xlab= xlab, ylab = ylab, ...)
  
  
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

setMethod("Arith", signature = c(e1 = "irregFunData", e2 = "irregFunData"),
          function(e1,e2){
            f <- function(x,y){methods::callGeneric(x,y)} # helper function (callGeneric not applicable in mapply)
            
            if(nObs(e1) != nObs(e2))
            {
              if(nObs(e1) == 1)
              {
                if(!all(unlist(e2@xVal) %in% unlist(e1@xVal)))
                  stop("Arithmetics: Multiple functions must be defined on subdomain of single function.")
                
                res <- irregFunData(xVal = e2@xVal, 
                                    X = sapply(1:nObs(e2), function(i){f(e1@X[[1]][which(e1@xVal[[1]] %in% e2@xVal[[i]])], e2@X[[i]])}))
              }
              else
              {
                if(nObs(e2) == 1)
                {
                  if(!all(unlist(e1@xVal) %in% unlist(e2@xVal)))
                    stop("Arithmetics: Multiple functions must be defined on subdomain of single function.")
                  
                  res <- irregFunData(xVal = e1@xVal,
                                      X = sapply(1:nObs(e1), function(i){f(e1@X[[i]], e2@X[[1]][which(e2@xVal[[1]] %in% e1@xVal[[i]])])}))
                }
                else
                  stop("Arithmethics: IrregFunData objects must have either the same number of observations or just one.")
              } 
            }            
            else
            {
              if(!isTRUE(all.equal(e1@xVal, e2@xVal)))
                stop("Arithmetics for two irregular functional data objects are defined only for functions on the same domain.")
              
              res <- irregFunData(xVal = e1@xVal, X = mapply(function(x,y){f(x,y)}, e1@X, e2@X) )
            }
            
            return(res)
          })



# @rdname Arith.funData
setMethod("Arith", signature = c(e1 = "irregFunData", e2 = "funData"),
          function(e1, e2){
            #  if(any(c(dimSupp(e1), dimSupp(e2)) != 1))
            #    stop("Arithmetic operations: defined only for irregFunData objects with one-dimensional domain")
            
            if(!any(unlist(e1@xVal) %in% e2@xVal[[1]]))
              stop("arithmetic operations: irregFunData object must be defined on a subdomain of the funData object!")
            
            # if funData object has only a single observation: apply to all of the other object
            if(nObs(e1) != nObs(e2))
            {
              if(nObs(e2) == 1 & nObs(e1) > 1) 
                e2@X <- t(replicate(nObs(e1),e2@X[1,]))            
              else
                stop("funData object must have either one observation or the same number of observations as the irregFunData object")
            }
            f <- function(x,y){methods::callGeneric(x,y)} # helper function (callGeneric not applicable in sapply)
            irregFunData(xVal = e1@xVal, X = sapply(1:nObs(e1), function(i){f(e1@X[[i]], e2@X[i,e2@xVal[[1]] %in% e1@xVal[[i]]])}, simplify = FALSE))
          })


setMethod("Arith", signature = c(e1 = "funData", e2 = "irregFunData"),
          function(e1, e2){
            #  if(any(c(dimSupp(e1), dimSupp(e2)) != 1))
            #    stop("Arithmetic operations: defined only for irregFunData objects with one-dimensional domain")
            
            if(!any(unlist(e2@xVal) %in% e1@xVal[[1]]))
              stop("arithmetic operations: irregFunData object must be defined on a subdomain of the funData object!")
            
            # if funData object has only a single observation: apply to all of the other object
            if(nObs(e1) != nObs(e2))
            {
              if(nObs(e1) == 1 & nObs(e2) > 1) 
                e1@X <- t(replicate(nObs(e2),e1@X[1,]))            
              else
                stop("funData object must have either one observation or the same number of observations as the irregFunData object")
            }
            f <- function(x,y){methods::callGeneric(x,y)} # helper function (callGeneric not applicable in sapply)
            irregFunData(xVal = e2@xVal, X = sapply(1:nObs(e2), function(i){f(e2@X[[i]], e1@X[i,e1@xVal[[1]] %in% e2@xVal[[i]]])}, simplify = FALSE))
          })






# #' @keywords internal
setMethod("norm", signature = "irregFunData",
          function(object, squared = TRUE, obs= 1:nObs(object), method = "trapezoidal", fullDom = FALSE){norm.irregFunData(object, squared, obs, method, fullDom)})


norm.irregFunData <- function(object, squared, obs, method, fullDom)
{
  object <- extractObs(object, obs)
  
  if(fullDom == TRUE) # extrapolate first
    object <- extrapolateIrreg(object)
  
  res <- integrate(object^2, method = method, fullDom = FALSE)
  
  if(!squared)
    res <- sqrt(res)
  
  return(res)
}

