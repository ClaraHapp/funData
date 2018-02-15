### Summary methods for functional data classes ###

setMethod("summary", signature= "funData",
          function(object, ...)
          {
            argvalsSummary <- do.call("rbind", lapply(object@argvals, summary, ...))
            rownames(argvalsSummary) <- paste("Dim.", 1:dimSupp(object), ":")
            
            XSummary <- apply(object@X, 1, function(x){summary(as.numeric(x))})
            colnames(XSummary) <- paste("Obs", 1:nObs(object))
            
            res <- list(argvals = argvalsSummary, X = XSummary)
            class(res) <- "summary.funData"
            
            return(res)
          })

setMethod("summary", signature= "multiFunData",
          function(object, ...)
          {
            res <- lapply(object, summary, ...)
            class(res) <- "summary.multiFunData"
            
            return(res)
          })


print.summary.funData <- function(x, ...)
{
  if(class(x) != "summary.funData")
    stop("Argument is not of class 'summary.funData'.")
  
  cat("Argument values (@argvals):\n")
  print(x$argvals)
  
  cat("\nObserved functions (@X):\n")
  print(x$X)
  
  cat("\n")
  invisible(x)
}

print.summary.multiFunData <- function(x, ...)
{
  if(class(x) != "summary.multiFunData")
    stop("Argument is not of class 'summary.multiFunData'.")
  
  sapply(1:length(x), function(i){
    cat("--- Element", i, " ---\n")
    print.summary.funData(x[[i]])})
  
  invisible(x)
}

