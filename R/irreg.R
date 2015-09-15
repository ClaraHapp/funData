# library(funData)

















setGeneric("nObsPoints", function(object) {standardGeneric("nObsPoints")})

setMethod("nObsPoints", signature = "irregFunData", 
          function(object){sapply(object@xVal, function(l){length(l)})})









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


