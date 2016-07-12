# library(funData)

#' @importFrom graphics points
plot.argvals <- function(x, y, type = "p", pch = 20,
                       col = grDevices::rainbow(nObs(x)), xlab = "argvals", ylab = "",
                       add = FALSE, xlim = range(x@argvals), ylim = c(1,nObs(x)), ...)
{
  
  plot(x = NULL, y = NULL, type = "n", xlim = xlim, ylim = ylim, xlab= xlab, ylab = ylab, ...)
  
  
  for(i in 1:nObs(x))
    graphics::points(x = x@argvals[[i]], y = rep(i, length(x@argvals[[i]])), type = type, pch = pch, col = col[i], ...)
  
}


