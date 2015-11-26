# library(funData)






























plot.argvalss <- function(x, y, type = "p", pch = 20,
                       col =rainbow(nObs(x)), xlab = "argvals", ylab = "",
                       add = FALSE, xlim = range(x@argvals), ylim = c(1,nObs(x)), ...)
{
  
  plot(x = NULL, y = NULL, type = "n", xlim = xlim, ylim = ylim, xlab= xlab, ylab = ylab, ...)
  
  
  for(i in 1:nObs(x))
    points(x = x@argvals[[i]], y = rep(i, length(x@argvals[[i]])), type = type, pch = pch, col = col[i], ...)
  
}


