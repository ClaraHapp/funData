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
#' \donttest{
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
#' }
plot.funData <- function(x, y, obs = 1:nObs(x), type = "l", lty = 1, lwd = 1,
                         col = NULL, xlab = "argvals", ylab = "", legend = TRUE,
                         plotNA = FALSE, add = FALSE, ...)
{
  # check arguments, which are not simply passed to other plot methods
  if(! all(is.numeric(obs), 0 < obs, obs <= nObs(x)))
    stop("Parameter 'obs' must be a vector of numerics with values between 1 and ", nObs(x), ".")
  if(! all(is.logical(plotNA), length(plotNA) == 1))
    stop("Parameter 'plotNA' must be passed as a logical.")
  if(! all(is.logical(add), length(add) == 1))
    stop("Parameter 'add' must be passed as a logical.")
  
  
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
      stop("Option add = TRUE not implemented for images")
    
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
#' This function plots observations of multivariate functional data on their domain. The graphic
#' device is split in a number of subplots (specified by \code{dim}) via \code{mfrow}
#' (\code{\link[graphics]{par}}) and the univariate elements are plotted using \code{plot}.
#' 
#' @section Warning: The function is currently implemented only for functional data with one- and
#'   two-dimensional domains.
#'   
#' @param x An object of class \code{multiFunData}.
#' @param y Missing.
#' @param obs A vector of numerics giving the observations to plot. Defaults to 
#'   all observations in \code{x}. For two-dimensional functions (images) 
#'   \code{obs} must have length 1.
#' @param dim The dimensions to plot. Defaults to \code{length(x)}, i.e. all 
#'   functions in \code{x} are plotted.
#' @param par.plot Graphic parameters to be passed to the plotting regions. The 
#'   option \code{mfrow} is ignored. Defaults to \code{NULL}. See 
#'   \code{\link[graphics]{par}} for details.
#' @param main A string vector, giving the title of the plot. Can have the same 
#'   length as \code{dim} (different titles for each dimension) or length 
#'   \code{1} (one title for all dimensions). Defaults to \code{names(x)}.
#' @param xlab,ylab The titles for x- and y-axis. Defaults to \code{"argvals"} 
#'   for the x-axis and no title for the y-axis for all elements. Can be 
#'   supplied as a vector of the same length as \code{dim} (one x-/y-lab for each 
#'   element) or a single string that is applied for all elements. See 
#'   \code{\link[graphics]{plot}} for details.
#' @param log A character string, specifying the axis that is to be logarithmic.
#'   Can be \code{""} (non-logarithmic axis), \code{"x", "y", "xy"} or 
#'   \code{"yx"}. Defaults to \code{""} for all plots. Can be supplied as a 
#'   vector of the same length as \code{dim} (one log-specification for each 
#'   element) or a single string that is applied for all elements. See 
#'   \code{\link[graphics]{plot.default}} for details.
#' @param ylim Specifies the limits of the y-Axis. Can be either \code{NULL} 
#'   (the default, limits are chosen automatically), a vector of length 2 
#'   (giving the minimum and maximum range for all elements at the same time) or
#'   a list of the same length as \code{dim} (specifying the limits for each
#'   element separately).
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
plot.multiFunData <- function(x, y, obs = 1:nObs(x), dim = 1:length(x), par.plot = NULL, main = names(x), 
                              xlab = "argvals", ylab = "", log = "", ylim = NULL, ...){
  
  if(! all(is.numeric(obs), 0 < obs, obs <= nObs(x)))
    stop("Parameter 'obs' must be a vector of numerics with values between 1 and ", nObs(x), ".")
  if(! all(is.numeric(dim), 0 < dim, dim <= length(x)))
    stop("Parameter 'dim' must be a vector of numerics with values between 1 and ", length(x), ".")
  if(! any(is.null(par.plot), is.list(par.plot)))
    stop("Parameter 'par.plot' must be either NULL or passed as a list.")
  
  if(!any(is.null(main), length(main) == c(1,length(x))))
      stop("Parameter 'main' must be either NULL or have lengths 1 or ", length(x), ".")
  if(length(main) == 1)
        main <- rep(main, length(dim))
  
  if(length(xlab) == 1)
    xlab <- rep(xlab, length(dim))
  
  if(length(ylab) == 1)
    ylab <- rep(ylab, length(dim))
  
  if(length(log) == 1)
    log <- rep(log, length(dim))
  
  if(!is.null(ylim)) # if ylim is not set by default...
  {
    if(!all(is.list(ylim), length(ylim) == length(dim))) # it can either be a list with separate values for each element
    {
      if(all(is.vector(ylim), length(ylim) == 2)) # or a vector with values to be used for all elements
        ylim <- rep(list(ylim), length(dim))
      else
        stop("The ylim argument must be either a vector (used for all elements) or a list with values for each element.")
    }  
  }


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
  
    # plot the univariate functions
  for(i in 1:length(dim))
    plot(x[[dim[i]]], obs = obs, main = main[i], xlab = xlab[i], ylab = ylab[i], log = log[i], ylim = ylim[[i]], ...)
  
  # if no par.plot specified: reset graphics parameters
  if(is.null(par.plot))
    par(oldPar)

  # return invisibly
  invisible()
}

#' Plotting irregular functional data
#' 
#' This function plots observations of irregular functional data on their domain.
#' 
#' @param x An object of class \code{irregFunData}.
#' @param y Missing.
#' @param obs A vector of numerics giving the observations to plot. Defaults to all observations in 
#'   \code{x}.
#' @param type The type of plot. Defaults to \code{"b"} (line and point plot). See 
#'   \code{\link[graphics]{plot}} for details.
#' @param pch The point type. Defaults to \code{20} (solid small circles). See 
#'   \code{\link[graphics]{par}} for details.
#' @param col The color of the functions. Defaults to the \code{\link[grDevices]{rainbow}} palette.
#' @param xlab,ylab The titles for x- and y-axis. Defaults to \code{"argvals"} for the x-axis and no
#'   title for the y-axis. See \code{\link[graphics]{plot}} for details.
#' @param xlim,ylim The limits for x- and y-axis. Defaults to the total range of the data that is to
#'   plot. See \code{\link[graphics]{plot}} for details.
#' @param log A character string, specifying the axis that is to be logarithmic. Can be \code{""} 
#'   (non-logarithmic axis, the default), \code{"x", "y", "xy"} or \code{"yx"}. See 
#'   \code{\link[graphics]{plot.default}} for details. This parameter is ignored, if \code{add =
#'   TRUE}.
#' @param add Logical. If \code{TRUE}, add to current plot (only for one-dimensional functions). 
#'   Defaults to \code{FALSE}.
#' @param ... Additional arguments to \code{\link[graphics]{plot}}.
#'   
#' @seealso \code{\link{plot.funData}}, \code{\linkS4class{irregFunData}}, 
#'   \code{\link[graphics]{plot}}
#'   
#' @importFrom grDevices rainbow
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
                              col = grDevices::rainbow(length(obs)), xlab = "argvals", ylab = "",
                              xlim = range(x@argvals[obs]), ylim = range(x@X[obs]),
                              log = "", add = FALSE, ...)
{
  # check arguments, which are not simply passed to other plot methods
  if(! all(is.numeric(obs), 0 < obs, obs <= nObs(x)))
    stop("Parameter 'obs' must be a vector of numerics with values between 1 and ", nObs(x), ".")
  if(! all(is.logical(add), length(add) == 1))
    stop("Parameter 'add' must be passed as a logical.")
  
  if(length(col) < length(obs))
    col <- rep(col,length(obs))
  
  if(add == FALSE) # plot new window
  {
    plot(x = xlim, y = ylim, type = "n", xlim = xlim, ylim = ylim,  xlab = xlab, ylab = ylab, log = log,...)
  }
  else # check if a log-parameter is passed, that does not match the current plot and give a warning
  {
    if((grepl(pattern = "y", x = log) != par()$ylog) | (grepl(pattern = "x", x = log) != par()$xlog))
      warning("Parameter 'log' cannot be reset when 'add = TRUE'.")
  }
  
  for(i in 1:length(obs))
    graphics::points(x = x@argvals[[obs[i]]], y = x@X[[obs[i]]], type = type, pch = pch, col = col[i], ...)
  
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
#' @param geom A character string describing the geometric object to use.
#'   Defaults to \code{"line"}. See \pkg{ggplot2} for details.
#' @param plotNA Logical. If \code{TRUE}, missing values are interpolated using 
#'   the \code{\link{approxNA}} function (only for one-dimensional functions). 
#'   Defaults to \code{FALSE}. See Details.
#' @param add Logical. If \code{TRUE}, add to current plot (only for 
#'   one-dimensional functions). Defaults to \code{FALSE}.
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
#' @examples
#' # Install package ggplot2 before running the examples
#' \dontshow{requireNamespace("ggplot2", quietly = TRUE)}
#' 
#' # One-dimensional
#' argvals <- seq(0,2*pi,0.01)
#' object <- funData(argvals,
#'                    outer(seq(0.75, 1.25, length.out = 11), sin(argvals)))
#' 
#' g <- ggplot(object) # returns ggplot object
#' g # plot the object
#' 
#' # add the mean function in red
#' g + ggplot(meanFunction(object), add = TRUE, col = 2)
#' 
#' # Two-dimensional
#' X <- array(0, dim = c(2, length(argvals), length(argvals)))
#' X[1,,] <- outer(argvals, argvals, function(x,y){sin((x-pi)^2 + (y-pi)^2)})
#' X[2,,] <- outer(argvals, argvals, function(x,y){sin(2*x*pi) * cos(2*y*pi)})
#' object2D <- funData(list(argvals, argvals), X)
#' 
#' \donttest{
#' ggplot(object2D, obs = 1)
#' ggplot(object2D, obs = 2)}
#' \dontrun{plot(object2D)} # must specify obs!
#' 
#' ### More examples ###
#' \donttest{
#' par(mfrow = c(1,1))
#' 
#' # using plotNA (needs packages zoo and gridExtra)
#' \dontshow{requireNamespace("zoo", quietly = TRUE)}
#' \dontshow{requireNamespace("gridExtra", quietly = TRUE)}
#' objectMissing <- funData(1:5, rbind(c(1, NA, 5, 4, 3), c(10, 9, NA, NA, 6)))
#' g1 <- ggplot(objectMissing) # the default
#' g2 <- ggplot(objectMissing, plotNA = TRUE) # requires zoo
#' 
#' gridExtra::grid.arrange(g1 + ggplot2::ggtitle("plotNA = FALSE (default)"),
#'                         g2 + ggplot2::ggtitle("plotNA = TRUE")) # requires gridExtra
#' 
#' # Customizing plots (see ggplot2 documentation for more details)
#' # parameters passed to geom_line are passed via the ... argument
#' gFancy <- ggplot(object, color = "red", linetype = 2) 
#' gFancy
#' 
#' # new layers can be added directly to the ggplot object
#' gFancy + ggplot2::theme_bw() # add new layers to the ggplot object
#' gFancy + ggplot2::ggtitle("Fancy Plot with Title and Axis Legends") + 
#'          ggplot2::xlab("The x-Axis") + ggplot2::ylab("The y-Axis")
#' 
#' ggplot(object2D, obs = 1) + ggplot2::ggtitle("Customized 2D plot") + ggplot2::theme_minimal() +
#'           ggplot2::scale_fill_gradient(high = "green", low = "blue", name = "Legend here")
#' }
ggplot.funData <- function(data, obs = 1:nObs(data), geom = "line", plotNA = FALSE, add = FALSE, ...)
{
  if(!(requireNamespace("ggplot2", quietly = TRUE) & requireNamespace("reshape2", quietly = TRUE)))
  {
    warning("Please install the ggplot2 and reshape2 packages to use the ggplot function for funDataObjects.")
    return()
  } 
  
  if(dimSupp(data) > 2)
    stop("ggplot is implemented only for functional data with one- or two-dimensional domain")
  
  if(! all(is.numeric(obs), 0 < obs, obs <= nObs(data)))
    stop("Parameter 'obs' must be a vector of numerics with values between 1 and ", nObs(data), ".")
  if(! all(is.logical(plotNA), length(plotNA) == 1))
    stop("Parameter 'plotNA' must be passed as a logical.")
  if(! all(is.logical(add), length(add) == 1))
    stop("Parameter 'add' must be passed as a logical.")
  
  if(dimSupp(data) == 1)
  {
    if(plotNA) # interpolate NA values
        data <- approxNA(data)

    meltData <- reshape2::melt(data@X[obs, , drop = FALSE], varnames = c("obsInd", "obsPointX"))
    meltData$argvals <- data@argvals[[1]][meltData$obsPointX]
    meltData$obsInd <- as.factor(meltData$obsInd)
    
    if(add == TRUE)
      p <- ggplot2::stat_identity(data = meltData, ggplot2::aes_string(x = "argvals", y = "value", group = "obsInd"),
                             geom = geom, ...)
    else
      p <- ggplot2::ggplot(data = meltData, ggplot2::aes_string(x = "argvals", y = "value", group = "obsInd")) +
      ggplot2::stat_identity(geom = geom, ...) + 
      ggplot2::ylab("") 
  }
  
  if(dimSupp(data) == 2)
  {
    if(length(obs) > 1)
      stop("plot: specify one observation for plotting")
    
    if(add == TRUE)
      stop("Option add = TRUE not implemented for images")
    
    meltData <- reshape2::melt(data@X[obs, , , drop = FALSE], varnames = c("obsInd", "obsPointX", "obsPointY"))
    meltData$argvalsX <- data@argvals[[1]][meltData$obsPointX]
    meltData$argvalsY <- data@argvals[[2]][meltData$obsPointY]
    meltData$obsInd <- as.factor(meltData$obsInd)
    
    p <- ggplot2::ggplot(meltData, ggplot2::aes_string(x = "argvalsX", y = "argvalsY")) + 
      ggplot2::geom_raster(ggplot2::aes_string(fill = "value"), ...) + 
      ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::labs(fill = "")
  }
  
  return(p)
}


#' Visualize multivariate functional data objects using ggplot
#' 
#' This function allows to plot \code{multiFunData} objects based on the \pkg{ggplot2} package. The 
#' function applies the \code{\link{ggplot.funData}} function to each element and returns either a 
#' combined plot with all elements plotted in one row or a list containing the different subplots as
#' \code{\link[ggplot2]{ggplot}} objects. The individual objects can be customized using all 
#' functionalities of the \pkg{ggplot2} package.
#' 
#' @section Warning: Currently, the function does not accept different parameters for the univariate
#'   elements.
#'   
#' @param data A \code{multiFunData} object that is to be plotted.
#' @param obs A vector of numerics giving the observations to plot. Defaults to all observations in 
#'   \code{data}. For two-dimensional functions (images) \code{obs} must have length 1.
#' @param dim The dimensions to plot. Defaults to \code{length(data)}, i.e. all functions in 
#'   \code{data} are plotted.
#' @param plotGrid Logical. If \code{TRUE}, the data is plotted using 
#'   \code{\link[gridExtra]{grid.arrange}} and the list of \code{\link[ggplot2]{ggplot}} objects is 
#'   returned invisibly. If \code{FALSE}, only the list of objects is returned. Defaults to 
#'   \code{FALSE}.
#' @param ... Further parameters passed to the univariate \code{\link{ggplot}} functions for 
#'   \code{funData} objects.
#'   
#' @return A list of \code{\link[ggplot2]{ggplot}} objects that are also printed directly as a grid 
#'   if \code{plotGrid = TRUE}.
#'   
#' @seealso \code{\linkS4class{multiFunData}}, \code{\link[ggplot2]{ggplot}}, 
#'   \code{\link{plot.multiFunData}}
#'   
#' @examples
#' # Install packages ggplot2 and gridExtra before running the examples
#' \dontshow{requireNamespace("ggplot2", quietly = TRUE); requireNamespace("gridExtra", quietly = TRUE)}
#' 
#' # One-dimensional elements
#' argvals <- seq(0, 2*pi, 0.01)
#' f1 <- funData(argvals, outer(seq(0.75, 1.25, length.out = 11), sin(argvals)))
#' f2 <- funData(argvals, outer(seq(0.75, 1.25, length.out = 11), cos(argvals)))
#' 
#' m1 <- multiFunData(f1, f2)
#' 
#' g <- ggplot(m1) # default
#' g[[1]] # plot first element
#' g[[2]] # plot second element
#' gridExtra::grid.arrange(grobs = g, nrow = 1) # requires gridExtra package
#' 
#' ggplot(m1, plotGrid = TRUE) # the same directly with plotGrid = TRUE
#' 
#' \donttest{
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
#' g2[[1]] <- g2[[1]] + ggplot2::ggtitle("First element") + ggplot2::theme_bw()
#' g2[[2]] <- g2[[2]] + ggplot2::ggtitle("Second element") + 
#'                      ggplot2::scale_fill_gradient(high = "green", low = "blue")
#' gridExtra::grid.arrange(grobs = g2, nrow = 1) # requires gridExtra package
#' }
ggplot.multiFunData <- function(data, obs = 1:nObs(data), dim = 1:length(data), plotGrid = FALSE, ...)
{
  if(! all(is.numeric(dim), 0 < dim, dim <= length(data)))
    stop("Parameter 'dim' must be a vector of numerics with values between 1 and ", length(data), ".")
  if(! all(is.logical(plotGrid), length(plotGrid) == 1))
    stop("Parameter 'plotGrid' must be passed as a logical.")
  
  p <- sapply(data[dim], ggplot.funData, obs = obs, ...,  simplify = FALSE)
  
  if(plotGrid)
  {
    if(!requireNamespace("gridExtra", quietly = TRUE))
    {
      warning("Please install the gridExtra package to use the ggplot function for multiFunDataObjects with plotGrid = TRUE.")
      return()
    } 
    
    gridExtra::grid.arrange(grobs = p, nrow = 1)
    invisible(p)
  }
  else
    return(p)
}

#' Visualize irregular functional data objects using ggplot
#' 
#' This function allows to plot \code{irregFunData} objects on their domain
#' based on the \pkg{ggplot2} package. The function provides a wrapper that
#' rearranges the data in a \code{irregFunData} object returns a basic
#' \code{\link[ggplot2]{ggplot}} object, which can be customized using all
#' functionalities of the \pkg{ggplot2} package.
#' 
#' @param data A \code{irregFunData} object.
#' @param obs A vector of numerics giving the observations to plot. Defaults to
#'   all observations in \code{data}. For two-dimensional functions (images)
#'   \code{obs} must have length 1.
#' @param geom A character string describing the geometric object to use.
#'   Defaults to \code{"line"}. See \pkg{ggplot2} for details.
#' @param add Logical. If \code{TRUE}, add to current plot (only for 
#'   one-dimensional functions). Defaults to \code{FALSE}.
#' @param ... Further parameters passed to \code{\link[ggplot2]{stat_identity}},
#'   e.g. \code{alpha, color, fill, linetype, size}).
#'   
#' @return A \code{\link[ggplot2]{ggplot}} object that can be customized using
#'   all functionalities of the \pkg{ggplot2} package.
#'   
#' @seealso \code{\linkS4class{irregFunData}}, \code{\link[ggplot2]{ggplot}}, 
#'   \code{\link{plot.irregFunData}}
#'   
#' @examples
#' # Install packages ggplot2 and gridExtra before running the examples
#' \dontshow{requireNamespace("ggplot2", quietly = TRUE)}
#' 
#' # Generate data
#' argvals <- seq(0,2*pi,0.01)
#' ind <- replicate(5, sort(sample(1:length(argvals), sample(5:10,1))))
#' object <- irregFunData(argvals = lapply(ind, function(i){argvals[i]}),
#'                   X = lapply(ind, function(i){sample(1:10,1) / 10 * argvals[i]^2}))
#' 
#' # Plot the data
#' ggplot(object)
#' 
#'  # Parameters passed to geom_line are passed via the ... argument
#' ggplot(object, color = "red", linetype = 3)
#' 
#' # Plot the data and add green dots for the 2nd function
#' ggplot(object) + ggplot(object, obs = 2, geom = "point", color = "green", add = TRUE)
#' 
#' # New layers can be added directly to the ggplot object using functions from the ggplot2 package
#' g <- ggplot(object)
#' g + ggplot2::theme_bw() + ggplot2::ggtitle("Plot with minimal theme and axis labels") +
#'     ggplot2::xlab("The x-Axis") + ggplot2::ylab("The y-Axis")
ggplot.irregFunData <- function(data, obs = 1:nObs(data), geom = "line", add = FALSE, ...)
{
  if(!(requireNamespace("ggplot2", quietly = TRUE) & requireNamespace("reshape2", quietly = TRUE)))
  {
    warning("Please install the ggplot2 and reshape2 packages to use the ggplot function for funDataObjects.")
    return()
  } 
  
  if(! all(is.numeric(obs), 0 < obs, obs <= nObs(data)))
    stop("Parameter 'obs' must be a vector of numerics with values between 1 and ", nObs(data), ".")
  if(! all(is.logical(add), length(add) == 1))
    stop("Parameter 'add' must be passed as a logical.")
  
  meltData <- reshape2::melt(data@X[obs])
  names(meltData)[2] <- "obsInd"
  meltData$argvals <- unlist(data@argvals[obs])
  meltData$obsInd <- as.factor(meltData$obsInd)
  
  if(add == TRUE)
    p <- ggplot2::stat_identity(data = meltData, ggplot2::aes_string(x = "argvals", y = "value", group = "obsInd"),
                           geom = geom, ...)
  else
  p <- ggplot2::ggplot(meltData, ggplot2::aes_string(x = "argvals", y = "value", group = "obsInd")) +
    ggplot2::stat_identity(geom = geom, ...) + 
    ggplot2::ylab("") 
  
  return(p)
}

#' ggplot Graphics for Functional Data Objects
#' 
#' The \code{funData} allows to plot \code{funData}, \code{multiFunData} and \code{irregFunData} 
#' objects via the \pkg{ggplot2} package. For Details, see the help pages of the class-specific plot
#' functions (see below).
#' 
#' @param data A \code{funData}, \code{multiFunData} or \code{irregFunData} object.
#' @param ... Further parameters passed to the class-specific methods.
#'   
#' @return A \code{\link[ggplot2]{ggplot} object}
#'   
#' @seealso \code{\link{ggplot.funData}} for \code{funData} objects,
#'   \code{\link{ggplot.multiFunData}} for \code{multiFunData} objects and
#'   \code{\link{ggplot.irregFunData}} for \code{irregFunData} objects.
setGeneric("ggplot", function(data,...) {standardGeneric("ggplot")})

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