### Plot methods for functional data objects ###

#### Standard Plot ####

#' Plotting univariate functional data
#' 
#' This function plots observations of univariate functional data on their 
#' domain.
#' 
#' If some observations contain missing values (coded via \code{NA}), the 
#' functions can be interpolated using the option \code{plotNA = TRUE}. This 
#' option relies on the \code{\link[zoo]{na.approx}} function in package 
#' \code{\link[zoo]{zoo}} and is currently implemented for one-dimensional 
#' functions only in the function \code{\link{approxNA}}.
#' 
#' @section Warning: The function is currently implemented only for functional 
#'   data with one- and two-dimensional domains.
#'   
#' @param x An object of class \code{funData}.
#' @param y Missing.
#' @param obs A vector of numerics giving the observations to plot. Defaults to 
#'   all observations in \code{x}. For two-dimensional functions (images) 
#'   \code{obs} must have length 1.
#' @param type The type of plot. Defaults to \code{"l"} (line plot). See 
#'   \code{\link[graphics]{plot}} for details.
#' @param lty The line type. Defaults to \code{1} (solid line). See 
#'   \code{\link[graphics]{par}} for details.
#' @param lwd The line width. Defaults to \code{1}. See
#'   \code{\link[graphics]{par}} for details.
#' @param col The color of the functions. If not supplied (\code{NULL}, default 
#'   value), one-dimensional functions are plotted in the 
#'   \code{\link[grDevices]{rainbow}} palette and two-dimensional functions are 
#'   plotted using \code{\link[fields]{tim.colors}} from package 
#'   \code{\link[fields]{fields-package}}.
#' @param xlab,ylab The titles for x- and y-axis. Defaults to \code{"argvals"}
#'   for the x-axis and no title for the y-axis. See
#'   \code{\link[graphics]{plot}} for details.
#' @param legend Logical. If \code{TRUE}, a color legend is plotted for 
#'   two-dimensional functions (images). Defaults to \code{TRUE}.
#' @param plotNA Logical. If \code{TRUE}, missing values are interpolated using
#'   the \link{approxNA} function (only for one-dimensional functions). Defaults
#'   to \code{FALSE}.
#' @param add Logical. If \code{TRUE}, add to current plot (only for 
#'   one-dimensional functions). Defaults to \code{FALSE}.
#' @param ... Additional arguments to \code{\link[graphics]{matplot} } 
#'   (one-dimensional functions) or \code{\link[fields]{image.plot}}/ 
#'   \code{\link[graphics]{image}} (two-dimensional functions).
#'   
#' @seealso \code{\linkS4class{funData}}, \code{\link[graphics]{matplot}}, 
#'   \code{\link[fields]{image.plot}}, \code{\link[graphics]{image}}
#'   
#' @importFrom grDevices rainbow
#' @importFrom graphics matplot image
#'   
#' @examples
#' oldpar <- par(no.readonly = TRUE)
#' 
#' # One-dimensional
#' argvals <- seq(0,2*pi,0.01)
#' object <- funData(argvals,
#'                    outer(seq(0.75, 1.25, length.out = 11), sin(argvals)))
#' 
#' plot(object, main = "One-dimensional functional data")
#' 
#' # Two-dimensional
#' X <- array(0, dim = c(2, length(argvals), length(argvals)))
#' X[1,,] <- outer(argvals, argvals, function(x,y){sin((x-pi)^2 + (y-pi)^2)})
#' X[2,,] <- outer(argvals, argvals, function(x,y){sin(2*x*pi) * cos(2*y*pi)})
#' object2D <- funData(list(argvals, argvals), X)
#' 
#' plot(object2D, main = "Two-dimensional functional data (obs 1)", obs = 1)
#' plot(object2D, main = "Two-dimensional functional data (obs 2)", obs = 2)
#' \dontrun{plot(object2D, main = "Two-dimensional functional data")} # must specify obs!
#' 
#' ### More examples ###
#' par(mfrow = c(1,1))
#' 
#' # using plotNA
#' if(requireNamespace("zoo", quietly = TRUE))
#' {
#' objectMissing <- funData(1:5, rbind(c(1, NA, 5, 4, 3), c(10, 9, NA, NA, 6)))
#' par(mfrow = c(1,2))
#' plot(objectMissing, type = "b", pch = 20, main = "plotNA = FALSE") # the default
#' plot(objectMissing, type = "b", pch = 20, plotNA = TRUE, main = "plotNA = TRUE") # requires zoo
#' }
#' 
#' # Changing colors
#' plot(object, main = "1D functional data in grey", col = "grey")
#' plot(object, main = "1D functional data in heat.colors", col = heat.colors(nObs(object)))
#' 
#' plot(object2D, main = "2D functional data in topo.colors", obs = 1, col = topo.colors(64))

#' par(oldpar)
plot.funData <- function(x, y, obs = 1:nObs(x), type = "l", lty = 1, lwd = 1,
                         col = NULL, xlab = "argvals", ylab = "", legend = TRUE,
                         plotNA = FALSE, add = FALSE, ...)
{
  if(dimSupp(x) > 2)
    stop("plot is implemented only for functional data with one- or two-dimensional domain")
  
  if(dimSupp(x) == 1)
  {
    # set default color
    if(is.null(col))
      col <-  grDevices::rainbow(length(obs))
    
    if(plotNA) # interpolate NA values
    {
        plot(approxNA(x), obs = obs, type = "l", lty = lty,  lwd = lwd, col = col, xlab = xlab, ylab = ylab, add = add, ...)
        add = TRUE
    }
    
    graphics::matplot(x = x@argvals[[1]], y = t(x@X[obs,, drop = FALSE]), type = type, lty = lty,  lwd = lwd, col = col, xlab = xlab, ylab = ylab, add = add, ...)
  }
  if(dimSupp(x) == 2)
  {
    if(length(obs) > 1)
      stop("plot: specify one observation for plotting")
    
    if(add == TRUE)
      stop("plot: add = TRUE not implemented for images")
    
    # set default color
    if(is.null(col))
      col <-  fields::tim.colors(64)
    
    if(legend == TRUE)
    {
      fields::image.plot(x = x@argvals[[1]], y = x@argvals[[2]], z = x@X[obs, ,], lty = lty, xlab = xlab, ylab = ylab, col = col, ...)
    }
    else
    {
      graphics::image(x = x@argvals[[1]], y = x@argvals[[2]], z = x@X[obs, ,], lty = lty, xlab = xlab, ylab = ylab, col = col, ...)
    }
    
    
  }
}


#' Plotting multivariate functional data
#' 
#' This function plots observations of multivariate functional data on their 
#' domain. The graphic device is split in a number of subplots (specified by 
#' \code{dim}) via \code{mfrow} (\code{\link[graphics]{par}}) and the univariate
#' elements are plotted using \code{plot}.
#' 
#' @section Warning: The function is currently implemented only for functional 
#'   data with one- and two-dimensional domains.
#'   
#' @param x An object of class \code{multiFunData}.
#' @param y Missing.
#' @param obs A vector of numerics giving the observations to plot. Defaults to 
#'   all observations in \code{x}. For two-dimensional functions (images) 
#'   \code{obs} must have length 1.
#' @param dim The dimensions to plot. Defaults to \code{length(x)}, i.e. all
#'   functions in \code{x} are plotted.
#' @param par.plot Graphic parameters to be passed to the plotting regions. The 
#'   option \code{mfrow} is ignored. Defaults to \code{FALSE}. See 
#'   \code{\link[graphics]{par}} for details.
#' @param main A string vector, giving the title of the plot. Can have the same 
#'   length as \code{dim} (different titles for each dimension) or length 
#'   \code{1} (one title for all dimensions). Defaults to \code{NULL}.
#' @param xlab,ylab The titles for x- and y-axis. Defaults to \code{"argvals"} for the 
#'   x-axis and no title for the y-axis for all elements. Can be supplied as a 
#'   vector of the same length as \code{x} (one x-/y-lab for each element) or a 
#'   single string that is applied for all elements. See 
#'   \code{\link[graphics]{plot}} for details.
#' @param ... Additional arguments to \code{plot}.
#'   
#' @seealso \code{\linkS4class{funData}}, \code{\linkS4class{multiFunData}}, 
#'   \code{\link{plot.funData}}
#'   
#' @examples
#' oldpar <- par(no.readonly = TRUE)
#' argvals <- seq(0, 2*pi, 0.1)
#' 
#' # One-dimensional elements
#' f1 <- funData(argvals, outer(seq(0.75, 1.25, length.out = 11), sin(argvals)))
#' f2 <- funData(argvals, outer(seq(0.75, 1.25, length.out = 11), cos(argvals)))
#' 
#' m1 <- multiFunData(f1, f2)
#' plot(m1, main = c("1st element", "2nd element")) # different titles
#' plot(m1, main = "Multivariate Functional Data") # one title for all
#' 
#' # Mixed-dimensional elements
#' X <- array(0, dim = c(11, length(argvals), length(argvals)))
#' X[1,,] <- outer(argvals, argvals, function(x,y){sin((x-pi)^2 + (y-pi)^2)})
#' g <- funData(list(argvals, argvals), X)
#' 
#' m2 <- multiFunData(f1, g)
#' # different titles and labels
#' plot(m2, main = c("1st element", "2nd element"), obs = 1,
#'      xlab = c("xlab1", "xlab2"), 
#'      ylab = "one ylab for all")
#' # one title for all
#' plot(m2, main = "Multivariate Functional Data", obs = 1) 
#' 
#' \dontrun{plot(m2, main = c("1st element", "2nd element")) # must specify obs!}
#' 
#' par(oldpar)
plot.multiFunData <- function(x, y, obs = 1:nObs(x), dim = 1:length(x), par.plot = NULL, main = NULL, 
                              xlab = "argvals", ylab = "", ...){
  
  if(length(xlab) == 1)
    xlab <- rep(xlab, length(x))
  
  if(length(ylab) == 1)
    ylab <- rep(ylab, length(x))
  
  
  # if no par.plot specified: get graphics parameters
  if(is.null(par.plot))
  {
    oldPar <- par(no.readonly = TRUE)
  }  else
  {
    par(par.plot)
  }
  
  # split screen
  par(mfrow = c(1,length(dim)))
  
  
  if(!is.null(main) & (length(main) == 1))
    main <- rep(main, length(dim))
  
  
  # plot the univariate functions
  for(i in dim)
    plot(x[[i]], obs = obs, main = main[i], xlab = xlab[i], ylab = ylab[i], ...)
  
  
  # if no par.plot specified: reset graphics parameters
  if(is.null(par.plot))
    par(oldPar)
}

#' Plotting irregular functional data
#' 
#' This function plots observations of irregular functional data on their 
#' domain.
#' 
#' @param x An object of class \code{irregFunData}.
#' @param y Missing.
#' @param obs A vector of numerics giving the observations to plot. Defaults to 
#'   all observations in \code{x}.
#' @param type The type of plot. Defaults to \code{"b"} (line and point plot).
#'   See \code{\link[graphics]{plot}} for details.
#' @param pch The point type. Defaults to \code{20} (solid small circles). See 
#'   \code{\link[graphics]{par}} for details.
#' @param col The color of the functions. Defaults to the 
#'   \code{\link[grDevices]{rainbow}} palette.
#' @param xlab,ylab The titles for x- and y-axis. Defaults to \code{"argvals"}
#'   for the x-axis and no title for the y-axis. See
#'   \code{\link[graphics]{plot}} for details.
#' @param xlim,ylim The limits for x- and y-axis. Defaults to the total range of
#'   the data that is to plot. See \code{\link[graphics]{plot}} for details.
#' @param add Logical. If \code{TRUE}, add to current plot (only for 
#'   one-dimensional functions). Defaults to \code{FALSE}.
#' @param ... Additional arguments to \code{\link[graphics]{plot}}.
#'   
#' @seealso \code{\link{plot.funData}}, \code{\linkS4class{irregFunData}},
#'   \code{\link[graphics]{plot}}
#'   
#'  @importFrom grDevices rainbow
#' @importFrom graphics points
#'   
#' @examples
#' oldpar <- par(no.readonly = TRUE)
#' 
#' # Generate data
#' argvals <- seq(0,2*pi,0.01)
#' ind <- replicate(5, sort(sample(1:length(argvals), sample(5:10,1))))
#' object <- irregFunData(argvals = lapply(ind, function(i){argvals[i]}),
#'                   X = lapply(ind, function(i){sample(1:10,1) / 10 * argvals[i]^2}))
#' 
#' plot(object, main = "Irregular functional data")
#' 
#' par(oldpar)
plot.irregFunData <- function(x, y, obs = 1:nObs(x), type = "b", pch = 20,
                              col = grDevices::rainbow(nObs(x)), xlab = "argvals", ylab = "",
                              xlim = range(x@argvals[obs]), ylim = range(x@X[obs]),
                              add = FALSE, ...)
{
  if(length(col) < nObs(x))
    col <- rep(col, nObs(x))
  
  if(add == FALSE) # plot new window
  {
    plot(x = NULL, y = NULL, type = "n", xlim = xlim, ylim = ylim,  xlab = xlab, ylab = ylab, ...)
  }
  
  for(i in obs)
    graphics::points(x = x@argvals[[i]], y = x@X[[i]], type = type, pch = pch, col = col[i], ...)
  
}

#' @rdname plot.funData
#'
#' @exportMethod plot
setMethod("plot", signature = signature(x = "funData", y = "missing"),
          function(x,y,...){plot.funData(x,y,...)})

#' @rdname plot.multiFunData
#'
#' @exportMethod plot
setMethod("plot", signature =  signature(x = "multiFunData", y = "missing"),
          function(x,y,...){plot.multiFunData(x,y,...)})

#' @rdname plot.irregFunData
#'
#' @exportMethod plot
setMethod("plot", signature = signature(x = "irregFunData", y = "missing"),
          function(x,y,...){plot.irregFunData(x,y,...)})


#### ggplot ####

#' Visualize functional data objects using ggplot
#' 
#' This function allows to plot \code{funData} objects based on the 
#' \pkg{ggplot2} package. The function provides a wrapper that rearranges the 
#' data in a \code{funData} object on a one- or two-dimensional domain and 
#' provides a basic \code{\link[ggplot2]{ggplot}} object, which can be 
#' customized using all functionalities of the \pkg{ggplot2} package.
#' 
#' If some observations contain missing values (coded via \code{NA}), the 
#' functions can be interpolated using the option \code{plotNA = TRUE}. This 
#' option relies on the \code{\link[zoo]{na.approx}} function in package 
#' \code{\link[zoo]{zoo}} and is currently implemented for one-dimensional 
#' functions only in the function \code{\link{approxNA}}.
#' 
#' @param data A \code{funData} object on a one- or two-dimensional domain.
#' @param obs A vector of numerics giving the observations to plot. Defaults to 
#'   all observations in \code{data}. For two-dimensional functions (images) 
#'   \code{obs} must have length 1.
#' @param plotNA Logical. If \code{TRUE}, missing values are interpolated using 
#'   the \code{\link{approxNA}} function (only for one-dimensional functions). 
#'   Defaults to \code{FALSE}. See Details.
#' @param ... Further parameters passed to \code{\link[ggplot2]{geom_line}} (for
#'   one dimensional domains, e.g. \code{alpha, color, fill, linetype, size}) or
#'   to \code{\link[ggplot2]{geom_raster}} (for two-dimensional domains, e.g.
#'   \code{hjust, vjust, interpolate}).
#'   
#' @return A \code{\link[ggplot2]{ggplot}} object that can be customized  using 
#'   all functionalities of the \pkg{ggplot2} package.
#'   
#' @seealso \code{\linkS4class{funData}}, \code{\link[ggplot2]{ggplot}}, 
#'   \code{\link{plot.funData}}
#'   
#' @importFrom ggplot2 ggplot
#' @importFrom reshape2 melt
#'   
#' @examples
#' # One-dimensional
#' argvals <- seq(0,2*pi,0.01)
#' object <- funData(argvals,
#'                    outer(seq(0.75, 1.25, length.out = 11), sin(argvals)))
#' 
#' g <- ggplot(object) # returns ggplot object
#' g # plot the object
#' 
#' # Two-dimensional
#' X <- array(0, dim = c(2, length(argvals), length(argvals)))
#' X[1,,] <- outer(argvals, argvals, function(x,y){sin((x-pi)^2 + (y-pi)^2)})
#' X[2,,] <- outer(argvals, argvals, function(x,y){sin(2*x*pi) * cos(2*y*pi)})
#' object2D <- funData(list(argvals, argvals), X)
#' 
#' ggplot(object2D, obs = 1)
#' ggplot(object2D, obs = 2)
#' \dontrun{plot(object2D)} # must specify obs!
#' 
#' ### More examples ###
#' par(mfrow = c(1,1))
#' 
#' # using plotNA
#' if(requireNamespace("zoo", quietly = TRUE) & requireNamespace("gridExtra", quietly = TRUE))
#' {
#' objectMissing <- funData(1:5, rbind(c(1, NA, 5, 4, 3), c(10, 9, NA, NA, 6)))
#' g1 <- ggplot(objectMissing) # the default
#' g2 <- ggplot(objectMissing, plotNA = TRUE) # requires zoo
#' 
#' grid.arrange(g1 + ggtitle("plotNA = FALSE (default)"), g2 + ggtitle("plotNA = TRUE")) # requires gridExtra
#' }
#' 
#' # Customizing plots (see ggplot2 documentation for more details)
#' # parameters passed to geom_line are passed via the ... argument
#' gFancy <- ggplot(object, color = "red", linetype = 2) # parameters for geom_line are directly passed via ...
#' gFancy
#' 
#' # new layers can be added directly to the ggplot object
#' gFancy + theme_bw() # add new layers to the ggplot object
#' gFancy + ggtitle("Fancy Plot with Title and Axis Legends") + xlab("The x-Axis") + ylab("The y-Axis")
#' 
#' ggplot(object2D, obs = 1) + scale_fill_gradient(high = "green", low = "blue", name = "Legend here") +
#'  ggtitle("Customized 2D plot") + theme_minimal()
ggplot.funData <- function(data, obs = 1:nObs(data), plotNA = FALSE, ...)
{
  if(dimSupp(data) > 2)
    stop("ggplot is implemented only for functional data with one- or two-dimensional domain")
  
  if(dimSupp(data) == 1)
  {
    if(plotNA) # interpolate NA values
        data <- approxNA(data)

    meltData <- reshape2::melt(data@X[obs, , drop = FALSE], varnames = c("obsInd", "obsPointX"))
    meltData$argvals <- data@argvals[[1]][meltData$obsPointX]
    
    p <- ggplot2::ggplot(data = meltData, aes(x = argvals, y = value, group = obsInd)) +
      geom_line(...) + 
      ylab("") 
  }
  
  if(dimSupp(data) == 2)
  {
    if(length(obs) > 1)
      stop("plot: specify one observation for plotting")
    
    meltData <- reshape2::melt(data@X[obs, , , drop = FALSE], varnames = c("obsInd", "obsPointX", "obsPointY"))
    meltData$argvalsX <- data@argvals[[1]][meltData$obsPointX]
    meltData$argvalsY <- data@argvals[[2]][meltData$obsPointY]
    
    p <- ggplot2::ggplot(meltData, aes(x = argvalsX, y = argvalsY)) + 
      geom_raster(aes(fill = value), ...) + 
      xlab("") + ylab("") + labs(fill = "")
  }
  
  return(p)
}


#' Visualize multivariate functional data objects using ggplot
#' 
#' This function allows to plot \code{multiFunData} objects based on the 
#' \pkg{ggplot2} package. The function applies the \code{\link{ggplot.funData}} 
#' function to each element and returns either a combined plot with all elements
#' plotted in one row or a list containing the different subplots as 
#' \code{\link[ggplot2]{ggplot}} objects. The individual objects can be 
#' customized using all functionalities of the \pkg{ggplot2} package.
#' 
#' @section Warning: Currently, the function does no accept parameters for 
#'   \code{\link[ggplot2]{geom_line}} (element with one-dimensional domain) or 
#'   \code{\link[ggplot2]{grom_raster}} (element with two-dimensional domain) as
#'   in the univariate case.
#'   
#' @param data A \code{multiFunData} object that is to be plotted.
#' @param obs A vector of numerics giving the observations to plot. Defaults to 
#'   all observations in \code{data}. For two-dimensional functions (images) 
#'   \code{obs} must have length 1.
#' @param dim The dimensions to plot. Defaults to \code{length(data)}, i.e. all 
#'   functions in \code{data} are plotted.
#' @param plotGrid Logical. If \code{TRUE}, the data is plotted using
#'   \code{\link[gridExtra]{arrangeGrid}} and the list of
#'   \code{\link[ggplot2]{ggplot}} objects is returned invisibly. If
#'   \code{FALSE}, only the list of objects is returned. Defaults to
#'   \code{FALSE}.
#'   
#' @return A list of \code{\link[ggplot2]{ggplot}} objects that are also printed directly as a grid if \code{plotGrid = TRUE}.
#' 
#' @importFrom gridExtra grid.arrange
#' 
#' @examples
#' 
#' # One-dimensional elements
#' f1 <- funData(argvals, outer(seq(0.75, 1.25, length.out = 11), sin(argvals)))
#' f2 <- funData(argvals, outer(seq(0.75, 1.25, length.out = 11), cos(argvals)))
#' 
#' m1 <- multiFunData(f1, f2)
#' 
#' g <- ggplot(m1) # default
#' g[[1]] # plot first element
#' g[[2]] # plot second element
#' gridExtra::grid.arrange(grobs = g, nrow = 1)
#' 
#' ggplot(m1, plotGrid = TRUE) # the same directly with plotGrid = TRUE
#' 
#' # Mixed-dimensional elements
#' X <- array(0, dim = c(11, length(argvals), length(argvals)))
#' X[1,,] <- outer(argvals, argvals, function(x,y){sin((x-pi)^2 + (y-pi)^2)})
#' f2 <- funData(list(argvals, argvals), X)
#' 
#' m2 <- multiFunData(f1, f2)
#' 
#' ggplot(m2, obs = 1, plotGrid = TRUE)
#' 
#' # Customizing plots (see ggplot2 documentation for more details)
#' g2 <- ggplot(m2, obs = 1)
#' g2[[1]] <- g2[[1]] + ggtitle("First element") + theme_bw()
#' g2[[2]] <- g2[[2]] + ggtitle("Second element") + scale_fill_gradient(high = "green", low = "blue")
#' gridExtra::grid.arrange(grobs = g2, nrow = 1)
ggplot.multiFunData <- function(data, obs = 1:nObs(data), dim = 1:length(data), plotGrid = FALSE)
{
  p <- sapply(data[dim], ggplot.funData, obs = obs, simplify = FALSE)
  
  if(plotGrid)
  {
    gridExtra::grid.arrange(grobs = p, nrow = 1)
    invisible(p)
  }
  else
    return(p)
}


ggplot.irregFunData <- function(data, obs = 1:nObs(data))
{
  meltData <- reshape2::melt(object@X[obs])
  names(meltData)[2] <- "obsInd"
  meltData$argvals <- unlist(object@argvals[obs])
  
  p <- ggplot2::ggplot(meltData, aes(x = argvals, y = value, group = obsInd)) +
    geom_line() + 
    ylab("") 
  
  return(p)
}


#' @rdname ggplot.funData
#'
#' @exportMethod ggplot
setMethod("ggplot", signature = signature(data = "funData"),
          function(data,...){ggplot.funData(data,...)})

#' @rdname ggplot.multiFunData
#'
#' @exportMethod ggplot
setMethod("ggplot", signature = signature(data = "multiFunData"),
          function(data,...){ggplot.multiFunData(data,...)})

#' @rdname ggplot.irregFunData
#'
#' @exportMethod ggplot
setMethod("ggplot", signature = signature(data = "irregFunData"),
          function(data,...){ggplot.irregFunData(data,...)})