### generic functions ###

#### show ####

#' A print method for univariate functional data
#'
#' This function prints basic information about a \code{funData} object. This is
#' the standard console output for \code{funData} objects.
#'
#' @param x A \code{funData} object.
#'
#' @keywords internal
print.funData <- function(x,...){
  cat("Functional data object with", nObs(x) ,"observations of", dimSupp(x) ,"- dimensional support\nargvals:\n")
  
  for(i in 1:dimSupp(x))
  {
    cat("\t")
    if(length(x@argvals[[i]]) > 5)
      cat(x@argvals[[i]][1], x@argvals[[i]][2], "...", x@argvals[[i]][length(x@argvals[[i]])])
    else
      cat(x@argvals[[i]])
    cat("\t\t(", length(x@argvals[[i]]), " sampling points)\n", sep = "")
  }
  
  cat("X:\n\tarray of size", paste(dim(x@X), collapse = " x "),"\n")
}

#' @describeIn funData Print basic information about the \code{funData} object
#'   in the console. The default console output for \code{funData} objects.
#'
#' @param object A \code{funData} object.
#'
#' @docType methods
#'
#' @exportMethod show
setMethod("show", signature = "funData",
          function(object){print.funData(object)})


#' A print method for irregular functional data
#'
#' This function prints basic information about a \code{irregFunData} object. This is
#' the standard console output for \code{irregFunData} objects.
#'
#' @param x An \code{irregFunData} object.
#'
#' @keywords internal
print.irregFunData <- function(x,...){
  cat("Irregular functional data object with", nObs(x) ,"observations of", dimSupp(x) ,"- dimensional support\n")
  
  cat("argvals:\n\tValues in ", paste(round(range(x@argvals),3), collapse = " ... "), ".\n", sep = "")
  
  cat("X:\n\tValues in ", paste(round(range(x@X),3), collapse = " ... "),".\n", sep = "")
  
  cat("Total:\n\t", length(unlist(x@argvals)), " observations on " , length(unique(unlist(x@argvals))), " different x-values (",
      paste(range(nObsPoints(x)), collapse = " - "), " per observation).\n", sep = "")
}


#' @describeIn irregFunData Print basic information about the \code{irregFunData} object
#'   in the console. The default console output for \code{irregFunData} objects.
#'
#' @param object An \code{irregFunData} object.
#'
#' @docType methods
#'
#' @exportMethod show
setMethod("show", signature = "irregFunData",
          function(object){print.irregFunData(object)})

#### dimSupp ####

#' Support dimension of functional data
#'
#' This function returns the support dimension of an object of class
#' \code{funData}, \code{irregFunData} or \code{multiFunData}.
#'
#' @param object An object of  class \code{funData}, \code{irregFunData} or \code{multiFunData}.
#'
#' @return If \code{object} is univariate (i.e. of class \code{funData} or \code{irregFunData}), the
#'   function returns the dimension of the support of \code{object}. If
#'   \code{object} is multivariate (i.e. of class \code{multiFunData}), the
#'   function returns a vector, giving the support dimension of each element.
#'
#' @seealso \linkS4class{funData}, \linkS4class{irregFunData} \linkS4class{multiFunData}
#'
#' @export dimSupp
#'
#' @examples
#' # Univariate (one-dimensional)
#' object1 <- funData(argvals = 1:5, X = rbind(1:5, 6:10))
#' dimSupp(object1)
#'
#' # Univariate (two-dimensional)
#' object2 <- funData(argvals = list(1:10, 1:5), X = array(rnorm(100), dim = c(2,10,5)))
#' dimSupp(object2)
#' 
#' # Univariate (irregular)
#' irregObject <- irregFunData(argvals = list(1:5, 2:4), X = list(2:6, 3:5))
#' dimSupp(irregObject)
#'
#' # Multivariate
#' multiObject <- multiFunData(object1, object2)
#' dimSupp(multiObject)
setGeneric("dimSupp", function(object) {standardGeneric("dimSupp")})


#' dimSupp for funData objects
#'
#' @keywords internal
setMethod("dimSupp", signature = "funData",
          function(object){length(object@argvals)})


#' dimSupp for multiFunData objects
#'
#' @keywords internal
setMethod("dimSupp", signature = "multiFunData",
          function(object){sapply(object, dimSupp, simplify = TRUE)})

#' dimSupp for irregular functional data objects
#'
#' @keywords internal
setMethod("dimSupp", signature = "irregFunData",
          function(object){length(dim(as.array(object@argvals[[1]])))})

#### Plot ####

#' Plotting univariate functional data
#'
#' This function plots observations of univariate functional data on their
#' domain.
#'
#' If some observations contain missing values (coded via \code{NA}), the
#' functions can be interpolated using the option \code{plotNA = TRUE}. This
#' option relies on the \code{\link[zoo]{na.approx}} function in package
#' \code{\link[zoo]{zoo}} and is currently implemented for one-dimensional
#' functions only.
#'
#' @section Warning:
#' The function is currently implemented only for functional data with one- and
#' two-dimensional domains.
#'
#' @param x An object of class \code{funData}.
#' @param y Missing.
#' @param obs A vector of numerics giving the observations to plot. Defaults to
#'   all observations in \code{x}. For two-dimensional functions (images)
#'   \code{obs} must have length 1.
#' @param type The type of plot. Defaults to "l" (line plot). See
#'   \code{\link[graphics]{plot}} for details.
#' @param lty The line type. Defaults to 1 (solid line). See
#'   \code{\link[graphics]{par}} for details.
#' @param lwd The line width. Defaults to 1. See \code{\link[graphics]{par}} for
#'   details.
#' @param col The color of the functions. If not supplied (\code{NULL}, default
#'   value), one-dimensional functions are plotted in the
#'   \code{\link[grDevices]{rainbow}} palette and two-dimensional functions are
#'   plotted using \code{\link[fields]{tim.colors}} from package
#'   \code{\link[fields]{fields-package}}.
#' @param xlab,ylab The titles for x- and y-axis. Defaults to "argvals" for the
#'   x-axis and no title for the y-axis. See \code{\link[graphics]{plot}} for
#'   details.
#' @param legend Logical. If \code{TRUE}, a color legend is plotted for
#'   two-dimensional functions (images). Defaults to \code{TRUE}.
#' @param plotNA Logical. If \code{TRUE}, missing values are interpolated (only
#'   for one-dimensional functions). Defaults to \code{FALSE}. See Details.
#' @param add Logical. If \code{TRUE}, add to current plot (only for
#'   one-dimensional functions). Defaults to \code{FALSE}.
#' @param ... Additional arguments to \code{\link[graphics]{matplot} }
#'   (one-dimensional functions) or
#'   \code{\link[fields]{image.plot}}/\code{\link[graphics]{image}}
#'   (two-dimensional functions).
#'
#' @seealso \linkS4class{funData}, \code{\link[graphics]{matplot}},
#'   \code{\link[fields]{image.plot}}, \code{\link[graphics]{image}}
#'
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
#' X[2,,] <- outer(argvals, argvals, function(x,y){sin(2*x*pi)*cos(2*y*pi)})
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
                         col =NULL, xlab = "argvals", ylab = "", legend = TRUE,
                         plotNA = FALSE, add = FALSE, ...)
{
  if(dimSupp(x) > 2)
    stop("plot is implemented only for functional data with one- or two-dimensional domain")
  
  if(dimSupp(x) == 1)
  {
    # set default color
    if(is.null(col))
      col <-  rainbow(length(obs))
    
    if(plotNA) # interpolate NA values
    {
      # require zoo
      if (requireNamespace("zoo", quietly = TRUE))
      {
        matplot(x = x@argvals[[1]], y = zoo::na.approx(t(x@X[obs,, drop = FALSE])), type = "l", lty = lty,  lwd = lwd, col = col, xlab = xlab, ylab = ylab, ...)
        
        add = TRUE # add the standard plot
      }
      else
        warning("Package zoo needed for interpolating missing values in plot for funData. Ignoring plotNA = TRUE.")
    }
    
    matplot(x = x@argvals[[1]], y = t(x@X[obs,, drop = FALSE]), type = type, lty = lty,  lwd = lwd, col = col, xlab = xlab, ylab = ylab, add = add, ...)
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
      image(x = x@argvals[[1]], y = x@argvals[[2]], z = x@X[obs, ,], lty = lty, xlab = xlab, ylab = ylab, col = col, ...)
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
#' @param dim The dimensions to plot. Defaults \code{length(x)}.
#' @param par.plot Graphic parameters to be passed to the plotting regions. The 
#'   option \code{mfrow} is ignored. Defaults to \code{FALSE}. See 
#'   \code{\link[graphics]{par}} for details.
#' @param main A string vector, giving the title of the plot. Can have the same 
#'   length as \code{dim} (different titles for each dimension) or length 
#'   \code{1} (one title for all dimensions). Defaults to \code{NULL}.
#' @param xlab,ylab The titles for x- and y-axis. Defaults to "argvals" for the 
#'   x-axis and no title for the y-axis for all elements. Can be supplied as a
#'   vector of the same length as \code{x} (one x-/y-lab for each element) or a
#'   single strin that is applied for all elements. See
#'   \code{\link[graphics]{plot}} for details.
#' @param ... Additional arguments to \code{plot}.
#'   
#' @seealso \linkS4class{funData}, \linkS4class{multiFunData}, 
#'   \code{\link{plot.funData}}
#'   
#' @examples
#' oldpar <- par(no.readonly = TRUE)
#' argvals <- seq(0, 2*pi, 0.1)
#' 
#' # One-dimensional elements
#' object1 <- funData(argvals, outer(seq(0.75, 1.25, length.out = 11), sin(argvals)))
#' object2 <- funData(argvals, outer(seq(0.75, 1.25, length.out = 11), cos(argvals)))
#' 
#' multiObject <- multiFunData(object1, object2)
#' plot(multiObject, main = c("1st element", "2nd element")) # different titles
#' plot(multiObject, main = "Multivariate Functional Data") # one title for all
#' 
#' # Mixed-dimensional elements
#' X <- array(0, dim = c(11, length(argvals), length(argvals)))
#' X[1,,] <- outer(argvals, argvals, function(x,y){sin((x-pi)^2 + (y-pi)^2)})
#' object2D <- funData(list(argvals, argvals), X)
#' 
#' multiObject <- multiFunData(object1, object2D)
#' # different titles and labels
#' plot(multiObject, main = c("1st element", "2nd element"), obs = 1,
#'      xlab = c("xlab1", "xlab2"), 
#'      ylab = "one ylab for all")
#' # one title for all
#' plot(multiObject, main = "Multivariate Functional Data", obs = 1) 
#' 
#' \dontrun{plot(multiObject, main = c("1st element", "2nd element")) # must specify obs!}
#' \dontrun{plot(multiObject, main = "Multivariate Functional Data") # must specify obs!}
#' 
#' par(oldpar)
plot.multiFunData <- function(x, y, obs = 1:nObs(x), dim = 1:length(x), par.plot = NULL, main = NULL, 
                              xlab = "argvals", ylab=  "", ...){
  
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
    plot(x[[i]], obs = obs, add = add, main = main[i], xlab= xlab[i], ylab = ylab[i], ...)
  
  
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
#' @param type The type of plot. Defaults to "b" (line and point plot). See 
#'   \code{\link[graphics]{plot}} for details.
#' @param pch The point type. Defaults to 20 (solid small circles). See 
#'   \code{\link[graphics]{par}} for details.
#' @param col The color of the functions. Defaults to the 
#'   \code{\link[grDevices]{rainbow}} palette.
#' @param xlab,ylab The titles for x- and y-axis. Defaults to "argvals" for the 
#'   x-axis and no title for the y-axis. See \code{\link[graphics]{plot}} for 
#'   details.
#' @param xlim,ylim The limits for x- and y-axis. Defaults to the total range of
#'   the data that is to plot. See \code{\link[graphics]{plot}} for details.
#' @param add Logical. If \code{TRUE}, add to current plot (only for 
#'   one-dimensional functions). Defaults to \code{FALSE}.
#' @param ... Additional arguments to \code{\link[graphics]{plot}}.
#'   
#' @seealso \code{\link{plot.funData}}, \linkS4class{irregFunData}, \code{\link[graphics]{plot}},    
#'   
#' @examples
#' oldpar <- par(no.readonly = TRUE)
#' 
#' # Generate data
#' argvals <- seq(0,2*pi,0.01)
#' ind <- replicate(5, sort(sample(1:length(argvals), sample(5:10,1))))
#' object <- irregFunData(argvals = lapply(ind, function(i){argvals[i]}),
#'                   X = lapply(ind, function(i){sample(1:10, 1)/10*argvals[i]^2}))
#' 
#' plot(object, main = "Irregular functional data")
#' 
#' par(oldpar)
plot.irregFunData <- function(x, y, obs = 1:nObs(x), type = "b", pch = 20,
                              col =rainbow(nObs(x)), xlab = "argvals", ylab = "",
                              xlim = range(x@argvals[obs]), ylim = range(x@X[obs]),
                              add = FALSE, ...)
{
  if(length(col) < nObs(x))
    col <- rep(col, nObs(x))
  
  if(add == FALSE) # plot new window
  {
    plot(x = NULL, y = NULL, type = "n", xlim = xlim, ylim = ylim,  xlab= xlab, ylab = ylab, ...)
  }
  
  for(i in obs)
    points(x = x@argvals[[i]], y = x@X[[i]], type = type, pch = pch, col = col[i], ...)
  
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


#### Arith ####

#' Arithmetics for functional data objects
#' 
#' These functions allow basic arithmetics for functional data and numerics 
#' based on \code{\link[methods]{Arith}}.  The operations are made pointwise for
#' each observation.
#' 
#' If two objects of a functional data class (\code{funData}, 
#' \code{irregFunData} or \code{multiFunData}) are used, they normally must be 
#' of the same class, have the same domain and the same number of observations. 
#' Exceptions are accepted if \itemize{\item one object has only one 
#' observation. In this case, the arithmetic operations are done pairwise for 
#' this single function and all functions of the other object (e.g. when 
#' subtracting the mean function from a \code{funData} object). This single
#' function must be defined on the same domain as the other functions (or, in
#' case of \code{irregFunData}, on the union of all observation grids). \item
#' one of the two objects is of class \code{irregFunData}. Then, the other
#' object can be of class \code{fundata}, too, if it is defined on the union of
#' all observation grids. The result is an \code{irregFunData} object which is
#' defined on the same observation grid as the original \code{irregFunData}
#' object.}
#' 
#' @section Warning: Note that not all combinations of operations and classes 
#'   make sense, e.g. \code{e1 ^ e2} is sensible if \code{e1} is of class 
#'   \code{funData}, \code{irregFunData} or \code{multiFunData} and \code{e2} is
#'   numeric. The reverse is not true.
#'   
#' @param e1,e2 Objects of class \code{funData}, \code{irregFunData}, 
#'   \code{multiFunData} or \code{numeric}. If two functional data objects are 
#'   used, they must be of the same class, have the same domain and the same 
#'   number of observations. For exceptions, see Details.
#'   
#' @return An object of the same functional data class as \code{e1} or 
#'   \code{e2}, respectively.
#'   
#' @seealso \linkS4class{funData}, \linkS4class{irregFunData}, \linkS4class{multiFunData}, 
#'   \link[methods]{Arith}
#'   
#' @name Arith.funData
#'   
#' @examples
#' oldpar <- par(no.readonly = TRUE)
#' par(mfrow = c(3,2), mar = rep(2.1,4))
#' 
#' argvals <- seq(0, 2*pi, 0.01)
#' object1 <- funData(argvals, outer(seq(0.75, 1.25, by = 0.05), sin(argvals)))
#' object2 <- funData(argvals, outer(seq(0.75, 1.25, by = 0.05), cos(argvals)))
#' 
#' plot(object1, main = "Object1")
#' plot(object2, main = "Object2")
#' 
#' # Only functional data objects
#' plot(object1 + object2, main = "Sum")
#' plot(object1 - object2, main = "Difference")
#' 
#' # Mixed
#' plot(4*object1 + 5,  main = "4*Object1 + 5") # Note y-axis!
#' plot(object1^2 + object2^2, main = "Pythagoras")
#' 
#' ### Irregular
#' ind <- replicate(11, sort(sample(1:length(argvals), sample(5:10, 1))))
#' i1 <- irregFunData(
#'    argvals = lapply(1:11, function(i, ind, x){x[ind[[i]]]}, ind = ind, x = object1@@argvals[[1]]),
#'    X = lapply(1:11, function(i, ind, y){y[i, ind[[i]]]}, ind = ind, y = object1@@X))
#' i2 <- irregFunData(
#'    argvals = lapply(1:11, function(i, ind, x){x[ind[[i]]]}, ind = ind, x = object2@@argvals[[1]]),
#'    X = lapply(1:11, function(i, ind, y){y[i, ind[[i]]]}, ind = ind, y = object2@@X))
#' 
#' plot(i1, main = "Object 1 (irregular)")
#' plot(i2, main = "Object 2 (irregular)")
#' 
#' # Irregular and regular functional data objects
#' plot(i1 + i2, main = "Sum")
#' plot(i1 - object2, main = "Difference")
#' 
#' # Mixed
#' plot(4*i1 + 5,  main = "4*i1 + 5") # Note y-axis!
#' plot(i1^2 + i2^2, main = "Pythagoras")
#' par(oldpar)
NULL

#' @rdname Arith.funData
#' 
#' @importFrom abind abind
setMethod("Arith", signature = c(e1 = "funData", e2 = "funData"),
          function(e1, e2){
            if(!all.equal(e1@argvals, e2@argvals))
              stop("Arithmetics: Functions must be defined on the same domain!")        
                   
            # different number of observations
            if(nObs(e1) != nObs(e2))
            {
              if(all(c(nObs(e1), nObs(e2)) > 1))
                stop("Arithmetics: nObs of funData objects is neither equal nor one.")
              
              # if one of e1, e2 has only one observation: replicate it nObs(e2) / nObs(e1) times 
              # is more efficient than aaply / apply
              if(nObs(e1) == 1 & nObs(e2) > 1) 
                e1@X <- do.call(abind::abind, args = list(rep(list(e1@X), nObs(e2)), along = 1) )
              
              if(nObs(e1) > 1 & nObs(e2) == 1) 
                e2@X <- do.call(abind::abind, args = list(rep(list(e2@X), nObs(e1)), along = 1) )  
            }
            
            funData(argvals = e1@argvals, X =  methods::callGeneric(e1@X, e2@X))
          })

#' @rdname Arith.funData
setMethod("Arith", signature = c(e1 = "funData", e2 = "numeric"),
          function(e1, e2) {
            funData(argvals = e1@argvals, X = methods::callGeneric(e1@X, e2))
          })

#' @rdname Arith.funData
setMethod("Arith", signature = c(e1 = "numeric", e2 = "funData"),
          function(e1, e2) {
            funData(argvals = e2@argvals, X = methods::callGeneric(e1, e2@X))
          })


#' @rdname Arith.funData
setMethod("Arith", signature = signature(e1 = "multiFunData", e2 = "multiFunData"),
          function(e1, e2) {
            if(length(e1) != length(e2))
              stop("arithmetic operations:multivariate functional data must have same length!")
            
            m <- vector("list", length(e1))
            for( i in 1:length(e1))
              m[[i]] <- methods::callGeneric(e1[[i]], e2[[i]])
            multiFunData(m)
          })

#' @rdname Arith.funData
setMethod("Arith", signature = signature(e1 = "multiFunData", e2 = "numeric"),
          function(e1, e2) {
            m <- vector("list", length(e1))
            for( i in 1:length(e1))
              m[[i]] <- methods::callGeneric(e1[[i]], e2)
            multiFunData(m)
          })

#' @rdname Arith.funData
setMethod("Arith", signature = signature(e1 = "numeric", e2 = "multiFunData"),
          function(e1, e2) {
            m <- vector("list", length(e2))
            for( i in 1:length(e2))
              m[[i]] <- methods::callGeneric(e1, e2[[i]])
            multiFunData(m)
          })


#' @rdname Arith.funData
setMethod("Arith", signature = c(e1 = "irregFunData", e2 = "numeric"),
          function(e1, e2) {
            f <- function(x,y){methods::callGeneric(x,y)} # helper function (callGeneric not applicable in lapply)
            irregFunData(argvals = e1@argvals, X = lapply(e1@X, function(x){f(x,e2)}))
          })

#' @rdname Arith.funData
setMethod("Arith", signature = c(e1 = "numeric", e2 = "irregFunData"),
          function(e1, e2) {
            f <- function(x,y){methods::callGeneric(x,y)} # helper function (callGeneric not applicable in lapply)
            irregFunData(argvals = e2@argvals, X = lapply(e2@X, function(x){f(e1,x)}))
          })

#' @rdname Arith.funData
setMethod("Arith", signature = c(e1 = "irregFunData", e2 = "irregFunData"),
          function(e1,e2){
            f <- function(x,y){methods::callGeneric(x,y)} # helper function (callGeneric not applicable in mapply)
            
            if(nObs(e1) != nObs(e2))
            {
              if(nObs(e1) == 1)
              {
                if(!all(unlist(e2@argvals) %in% unlist(e1@argvals)))
                  stop("Arithmetics: Multiple functions must be defined on subdomain of single function.")
                
                res <- irregFunData(argvals = e2@argvals, 
                                    X = sapply(1:nObs(e2), function(i){f(e1@X[[1]][which(e1@argvals[[1]] %in% e2@argvals[[i]])], e2@X[[i]])}))
              }
              else
              {
                if(nObs(e2) == 1)
                {
                  if(!all(unlist(e1@argvals) %in% unlist(e2@argvals)))
                    stop("Arithmetics: Multiple functions must be defined on subdomain of single function.")
                  
                  res <- irregFunData(argvals = e1@argvals,
                                      X = sapply(1:nObs(e1), function(i){f(e1@X[[i]], e2@X[[1]][which(e2@argvals[[1]] %in% e1@argvals[[i]])])}))
                }
                else
                  stop("Arithmethics: IrregFunData objects must have either the same number of observations or just one.")
              } 
            }            
            else
            {
              if(!isTRUE(all.equal(e1@argvals, e2@argvals)))
                stop("Arithmetics for two irregular functional data objects are defined only for functions on the same domain.")
              
              res <- irregFunData(argvals = e1@argvals, X = mapply(function(x,y){f(x,y)}, e1@X, e2@X) )
            }
            
            return(res)
          })

#' @rdname Arith.funData
setMethod("Arith", signature = c(e1 = "irregFunData", e2 = "funData"),
          function(e1, e2){
            #  if(any(c(dimSupp(e1), dimSupp(e2)) != 1))
            #    stop("Arithmetic operations: defined only for irregFunData objects with one-dimensional domain")
            
            if(!any(unlist(e1@argvals) %in% e2@argvals[[1]]))
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
            irregFunData(argvals = e1@argvals, X = sapply(1:nObs(e1), function(i){f(e1@X[[i]], e2@X[i,e2@argvals[[1]] %in% e1@argvals[[i]]])}, simplify = FALSE))
          })

#' @rdname Arith.funData
setMethod("Arith", signature = c(e1 = "funData", e2 = "irregFunData"),
          function(e1, e2){
            #  if(any(c(dimSupp(e1), dimSupp(e2)) != 1))
            #    stop("Arithmetic operations: defined only for irregFunData objects with one-dimensional domain")
            
            if(!any(unlist(e2@argvals) %in% e1@argvals[[1]]))
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
            irregFunData(argvals = e2@argvals, X = sapply(1:nObs(e2), function(i){f(e2@X[[i]], e1@X[i,e1@argvals[[1]] %in% e2@argvals[[i]]])}, simplify = FALSE))
          })

#### nObs ####

#' Get the number of observations
#'
#' This functions returns the number of observations in a \code{funData}, \code{irregFunData} or \code{multiFunData} object.
#'
#' @param object An object of class \code{funData}, \code{irregFunData} or \code{multiFunData}.
#'
#' @return The number of observations in \code{object}.
#'
#' @seealso \linkS4class{funData}, \linkS4class{irregFunData}, \linkS4class{multiFunData}
#'
#' @export nObs
#'
#' @examples
#' # Univariate
#' object <- funData(argvals = 1:5, X = rbind(1:5, 6:10))
#' nObs(object)
#' 
#' # Univariate (irregular)
#' irregObject <- irregFunData(argvals = list(1:5, 2:4), X = list(2:6, 3:5))
#' nObs(irregObject)
#'
#' # Multivariate
#' multiObject <- multiFunData(object, funData(argvals = 1:3, X = rbind(3:5, 6:8)))
#' nObs(multiObject)
setGeneric("nObs", function(object) {standardGeneric("nObs")})

#' nObs for funData objects
#'
#' @keywords internal
setMethod("nObs", signature = "funData",
          function(object){dim(object@X)[1]})

#' nObs for multiFunData objects
#'
#' @keywords internal
setMethod("nObs", signature = "multiFunData",
          function(object){nObs(object[[1]])})

#' nObs for irregular functional data objects
#'
#' @keywords internal
setMethod("nObs", signature = "irregFunData",
          function(object){length(object@X)})

#### nObsPoints ####

#' Get the number of observation points
#' 
#' This functions returns the number of observation points in an object of class
#' \code{funData}, \code{multiFunData} or \code{irregFunData}.
#' 
#' Depending on the class of \code{object}, the function returns different 
#' values: \itemize{ \item If \code{object} is of class \code{funData}, the 
#' function returns a vector of length \code{dimSupp(object)}, giving the number
#' of observations in each dimension. \item If \code{object} is of class 
#' \code{multiFunData}, the function returns a list of the same length as 
#' \code{object}, where the \code{j}-th entry is a vector, corresponding to the 
#' observations point of \code{object[[j]]}. \item If \code{object} is of class
#' \code{irregFunData}, the function returns an array of length
#' \code{nObs(object)}, where the \code{j}-th entry corresponds to the number of
#' observations in the \code{j}-th observed function.}
#' 
#' @section Warning: Do not confound with \code{\link{nObs}}, which returns the 
#'   number of observations (i.e. the number of observed functions) in an object
#'   of a functional data class.
#'   
#' @param object An object of class \code{funData}, \code{multiFunData} or 
#'   \code{irregFunData}.
#'   
#' @return The number of observation points in \code{object}. See Details.
#'   
#' @seealso \linkS4class{irregFunData}, \code{\link{extractObs}}
#'   
#' @export nObsPoints
#'   
#' @examples
#' # Univariate (one-dimensional)
#' object1 <- funData(argvals = 1:5, X = rbind(1:5, 6:10))
#' nObsPoints(object1)
#' 
#' # Univariate (two-dimensional)
#' object2 <- funData(argvals = list(1:5, 1:6), X = array(1:60, dim = c(2, 5, 6)))
#' nObsPoints(object2)
#' 
#' # Multivariate
#' multiObject <- multiFunData(object1, object2)
#' nObsPoints(multiObject)
#' 
#' # Univariate (irregular)
#' irregObject <- irregFunData(argvals = list(1:5, 2:4), X = list(2:6, 3:5))
#' nObsPoints(irregObject)
setGeneric("nObsPoints", function(object) {standardGeneric("nObsPoints")})

#' nObsPoints for funData objects
#'
#' @keywords internal
setMethod("nObsPoints", signature = "funData", 
          function(object){sapply(object@argvals, length)})

#' nObsPoints for multiFunData objects
#'
#' @keywords internal
setMethod("nObsPoints", signature = "multiFunData", 
          function(object){lapply(object, nObsPoints)})

#' nObsPoints for irregular functional data objects
#'
#' @keywords internal
setMethod("nObsPoints", signature = "irregFunData", 
          function(object){sapply(object@argvals, length)})

#### extractObs ####

#' Extract observations of functional data
#' 
#' This function extracts one or more observations and/or observations on a part
#' of the domain from a \code{funData}, \code{irregFunData} or
#' \code{multiFunData} object.
#' 
#' In case of an \code{irregFunData} object, some functions may not have
#' observation points in the given part of the domain. In this case, the
#' functions are removed from the extracted dataset and a warning is thrown.
#' 
#' @section Warning: The function is currently implemented only for functional
#'   data with up to three-dimensional domains.
#'   
#' @param object An object of class \code{funData}, \code{irregFunData} or
#'   \code{multiFunData}.
#' @param obs A numeric vector, giving the indices of the observations to 
#'   extract (default: all obervations).
#' @param argvals The part of the domain to be extracted (default: the whole domain
#'   \code{object}@@\code{argvals}). Must be a list or a numeric vector (only for one-dimensional
#'   domains, see also the definition of \linkS4class{funData}, 
#'   \linkS4class{multiFunData}).
#'   
#' @return An object of class \code{funData}, \code{irregFunData} or \code{multiFunData} containing 
#'   the desired observations.
#'   
#' @seealso \linkS4class{funData}, \linkS4class{irregFunData}, \linkS4class{multiFunData}
#'   
#' @export extractObs
#'   
#' @examples
#' # Univariate - one-dimensional domain
#' object1 <- funData(argvals = 1:5, X = rbind(1:5, 6:10))
#' extractObs(object1, obs = 1)
#' extractObs(object1, argvals = 1:3)
#' extractObs(object1, argvals = list(1:3)) # the same as the statement before
#' 
#' # Univariate - two-dimensional domains
#' object2 <- funData(argvals = list(1:5, 1:6), X = array(1:60, dim = c(2, 5, 6)))
#' extractObs(object2, obs = 1)
#' extractObs(object2, argvals = list(1:3, c(2,4,6))) # argvals must be supplied as list
#' 
#' # Univariate - irregular
#' irregObject <- irregFunData(argvals = list(1:5, 2:4), X = list(2:6, 3:5))
#' extractObs(irregObject, obs = 2)
#' extractObs(irregObject, argvals = 1:3)
#' extractObs(irregObject, argvals = c(1,5)) # throws a warning, as second function has no observations
#' 
#' # Multivariate
#' multiObject <- multiFunData(object1, object2)
#' extractObs(multiObject, obs = 2)
#' extractObs(multiObject, argvals = list(1:3, list(1:3, c(2,4,6))))
setGeneric("extractObs", function(object, obs = 1:nObs(object), argvals= getArgvals(object)) {
  standardGeneric("extractObs")
})

#' extractObs for funData objects
#'
#' @keywords internal
setMethod("extractObs", signature = signature("funData", "ANY", "ANY"),
          function(object, obs, argvals){
            
            if(dimSupp(object) > 3)
              stop("extracting observations is not implemented yet for functional data of dimension > 3")
            
            if(!is.numeric(obs))
              stop("Supply observations as numeric vector")
            
            if(!all((1:nObs(object))[obs] %in% 1:nObs(object)))
              stop("Trying to extract observations that do not exist!")
            
            if(!is.list(argvals))
            {
              if(dimSupp(object) == 1 & is.numeric(argvals))
                argvals = list(argvals)
              else
                stop("Supply argvals for extracted observations either as list or as numeric vector (only if support is one-dimensional)")
            }
            
            if(!all(unlist(mapply(function(x,y){x%in%y}, argvals,object@argvals))))
              stop("Trying to extract x-values that do not exist!")
            
            if(dimSupp(object) == 1)
              return(funData(argvals, object@X[obs,
                                            object@argvals[[1]] %in% argvals[[1]], drop = FALSE]))
            
            if(dimSupp(object) == 2)
              return(funData(argvals, object@X[obs,
                                            object@argvals[[1]] %in% argvals[[1]],
                                            object@argvals[[2]] %in% argvals[[2]], drop = FALSE]))
            
            if(dimSupp(object) == 3)
              return(funData(argvals, object@X[obs,
                                            object@argvals[[1]] %in% argvals[[1]],
                                            object@argvals[[2]] %in% argvals[[2]], 
                                            object@argvals[[3]] %in% argvals[[3]], drop = FALSE]))
          })

#' extractObs for multiFunData objects
#'
#' @keywords internal
setMethod("extractObs", signature = signature("multiFunData", "ANY", "ANY"), def = function(object, obs, argvals){
  if(!missing(argvals) & !is.list(argvals))
    stop("extractObs for multiFunData: argvals must be supplied as list (or missing).")
  if(is.list(obs))
    res <-   multiFunData(mapply(extractObs, object = object, obs = obs, argvals = argvals))
  else
    res <-  multiFunData(mapply(extractObs, object = object, argvals = argvals, MoreArgs = list(obs = obs)))
})

#' extractObs for irregular functional data
#' 
#' @keywords internal
setMethod("extractObs", signature = signature("irregFunData", "ANY", "ANY"),
          function(object, obs, argvals){
            #  if(dimSupp(object) > 1)
            #    stop("Extracting observations is not implemented yet for functional data of dimension > 1")
            
            if(!is.numeric(obs))
              stop("Supply observations as numeric vector")
            
            if(!all((1:nObs(object))[obs] %in% 1:nObs(object)))
              stop("Trying to extract observations that do not exist!")
            
            if(!is.list(argvals))
            {
              if(is.numeric(argvals))
                argvals = list(argvals)
              else
                stop("Supply argvals for extracted observations either as list or as numeric vector")
            }
            
            if(!any(unlist(argvals) %in% unlist(object@argvals[obs])))
              stop("Trying to extract x-values that do not exist!")
            
            extractargvals <- extractX <- vector("list", length(obs))
            omit <- NULL
            
            for(i in 1:length(obs))
            {
              ind <- which(object@argvals[[obs[i]]] %in% unlist(argvals))
              
              if(length(ind) == 0)
              {
                warning("Some functions were not observed on the given argvals and therefore removed.")
                
                omit <- c(omit, i)
              }
              
              extractargvals[[i]] <- object@argvals[[obs[i]]][ind]
              extractX[[i]] <- object@X[[obs[i]]][ind]
            }
            
            # omit empty observations
            extractargvals[omit] <- NULL
            extractX[omit] <- NULL
            
            return(irregFunData(extractargvals, extractX))
          })


#### integrate ####

#' Integrate functional data
#' 
#' Integrate all observations of a \code{funData}, \code{irregFunData} or 
#' \code{multiFunData} object over their domain.
#' 
#' Further parameters passed to this function may include: \itemize{ \item 
#' \code{method}: Character string. The integration rule to be used, passed to
#' the internal function \code{.intWeights}. Defaults to 'trapezoidal'
#' (alternative: 'midpoint'). \item \code{fullDom}: Logical. If \code{object} is
#' of class \code{irregFunData}, setting fullDom = TRUE extrapolates all
#' functions linearly to the full domain before calculating the integrals. Defaults to
#' \code{FALSE}. For details on the extrapolation, see 
#' \code{\link{extrapolateIrreg}}.}
#' 
#' @section Warning: The function is currently implemented only for functional 
#'   data with up to three-dimensional domains.
#'   
#' @param object An object of class \code{funData}, \code{irregFunData} or \code{multiFunData}.
#' @param ... Further parameters (see Details).
#'   
#' @return A vector of numerics, containing the integral values for each 
#'   observation.
#'   
#' @seealso \linkS4class{funData}, \linkS4class{irregFunData}, \linkS4class{multiFunData}
#'   
#' @export integrate
#'   
#' @examples
#' # Univariate
#' object <- funData(argvals = 1:5, X = rbind(1:5, 6:10))
#' integrate(object)
#' 
#' # Univariate (irregular)
#' irregObject <-irregFunData(argvals = list(1:5, 2:4), X = list(2:6, 3:5))
#' integrate(irregObject) # fullDom = FALSE
#' integrate(irregObject, fullDom = TRUE)
#' 
#' # Multivariate
#' multiObject <- multiFunData(object, funData(argvals = 1:3, X = rbind(3:5, 6:8)))
#' integrate(multiObject)
setGeneric("integrate", function(object, ...) {standardGeneric("integrate")})

# integration weights (internal functions)

#' Calculate weights for numerical integration
#'
#' This function calculates the weights for numerical integration
#'
#' @param argvals A numeric vector of x-Values
#' @param method A character string, giving the numerical integration method to use (default is \code{trapezoidal}, alternatively use \code{midpoint})
#'
#' @return A vector of integration weights
#'
#' @seealso \ref{integrate}
#'
#' @keywords internal
.intWeights <- function(argvals, method = "trapezoidal")
{
  if(method == "trapezoidal" & (length(argvals) < 3))
  {
    method <- "midpoint"
    warning("Trapezoidal quadrature is not applicable for functions with < 3 observation points. 'method' changed to 'midpoint'.")
  } 
  
  ret <- switch(method,
                midpoint = c(0,diff(argvals)),
                trapezoidal = {D <- length(argvals)
                               1/2*c(argvals[2] - argvals[1], argvals[3:D] -argvals[1:(D-2)], argvals[D] - argvals[D-1])},
                stop("function intWeights: choose either midpoint or trapezoidal quadrature rule"))
  
  return(ret)
}

#' Integrate a function on a rectangular 3D grid
#' 
#' @param f A 3D array, representing the function evaluations on the grid
#' @param argvals A list with 3 elements, representing the grid points in the first, second and third dimension of f
#' 
#' @return The result of the numerical integration of f on the given grid.
#' 
#' @keywords internal
integrate3D <- function(f, argvals)
{
  res <- t(.intWeights(argvals[[1]])) %*% apply(f, 1:2, function(w){w %*% .intWeights(argvals[[3]])}) %*% .intWeights(argvals[[2]])
  
  return(res)
}

#' Integrate method for funData objects
#'
#' @seealso \link{integrate} \linkS4class{funData}
#'
#' @keywords internal
setMethod("integrate", signature = "funData",
          function(object, method = "trapezoidal"){
            if(dimSupp(object) > 3)
              stop("Integration is not yet defined for functional data objects with dim > 3")
            
            if(dimSupp(object) == 1)
              res <- object@X %*% .intWeights(object@argvals[[1]],method)
            
            if(dimSupp(object) == 2)
              res <- apply( object@X , 1, function(x){t(.intWeights(object@argvals[[1]])) %*% x %*% .intWeights(object@argvals[[2]])})
            
            if(dimSupp(object) == 3)
              res <- apply(object@X, 1, integrate3D, argvals = object@argvals)
            
            return(as.numeric(res))
          })

#' Integrate method for multiFunData objects
#'
#' @seealso \link{integrate} \linkS4class{multiFunData}
#'
#' @keywords internal
setMethod("integrate", signature = "multiFunData",
          function(object, ...){
            uniIntegrate <- sapply(object, integrate, ...)
            
            if(nObs(object) == 1)
              res <- sum(uniIntegrate)
            else
              res <- rowSums(uniIntegrate)
            
            return(res)
          })


#' Integrate method for irregular functional data objects
#'
#' @seealso \link{integrate} \linkS4class{irregFunData}
#'
#' @keywords internal
setMethod("integrate", signature = c(object = "irregFunData"),
          function(object, method = "trapezoidal", fullDom = FALSE){
            if(fullDom) # fullDomain: extrapolate each function linearly (or by a constant, if only one value is observed)
              object <- extrapolateIrreg(object)
            
            return(mapply(function(x,y, method){sum(.intWeights(x, method)*y)}, 
                          x = object@argvals, y = object@X, MoreArgs = list(method = method)))
          })


#### extrapolateIrreg ####

#' Extrapolate irregular functional data to a given domain
#' 
#' This function extrapolates an \code{irregFunData} object to a given domain.
#' If only one point is observed, the function is extrapolated as a constant; in
#' all other cases it is extrapolated linearly.
#' 
#' @keywords internal
extrapolateIrreg <- function(object, rangex = range(object@argvals))
{  
  for(i in 1:nObs(object))
  {
    e <- .extrapolate(object@argvals[[i]], object@X[[i]], rangex)
    object@argvals[[i]] <- e$x
    object@X[[i]] <- e$y
  } 
  
  return(object)
}

# extrapolate function linearly (or set constant, if only one value is observed)
.extrapolate <- function(x,y, xrange)
{
  # add minimum and maximum observation point (no matter if already present)
  n <- length(x)
  
  if(n == 1)
    y <- rep(x,3)
  else
    y <- c(y[1] + (y[2] - y[1])/(x[2] - x[1])*(xrange[1] - x[1]),
           y,
           y[n-1] + (y[n] - y[n-1])/(x[n] - x[n-1])*(xrange[2] - x[n-1]))
  
  x <- c(xrange[1], x, xrange[2])
  
  return(list(x=x, y=y))
}


#### norm ####

#' Calculate the norm of functional data
#' 
#' This function calculates the norm for each observation of a \code{funData}, 
#' \code{irregFunData} or \code{multiFunData} object.
#' 
#' Further parameters passed to this function may include: \itemize{ \item 
#' \code{squared}: Logical. If \code{TRUE} (default), the function calculates 
#' the squared norm, otherwise the result is not squared. \item \code{obs}: A 
#' numeric vector, giving the indices of the observations, for which the norm is
#' to be calculated. Defaults to all observations. \item \code{method}: A 
#' character string, giving the integration method to be used. See 
#' \link{integrate} for details. \item \code{weight}: An optional vector of
#' weights for the scalar product; particularly useful for multivariate
#' functional data, where each entry can be weighted in the scalar product /
#' norm. Defaults to 1 for each element. \item \code{fullDom}: Logical. If
#' \code{object} is of class \linkS4class{irregFunData} and \code{fullDom =
#' TRUE}, all functions are extrapolated to the same domain. Defaults to
#' \code{FALSE}. See \link{integrate} for details. }
#' 
#' @section Warning: The function is currently implemented only for functional 
#'   data with one- and two-dimensional domains.
#'   
#' @param object An object of class \code{funData}, \code{irregFunData} or
#'   \code{multiFunData}.
#' @param ... Further parameters (see Details).
#'   
#' @return A numeric vector representing the norm of each observation.
#'   
#' @seealso \linkS4class{funData}, \linkS4class{irregFunData},
#'   \linkS4class{multiFunData}, \code{\link{integrate}}
#'   
#' @export norm
#'   
#' @examples
#' # Univariate
#' object <- funData(argvals = 1:5, X = rbind(1:5, 6:10))
#' norm(object)
#' 
#' # Univariate (irregular)
#' irregObject <- irregFunData(argvals = list(1:5, 2:4), X = list(2:6, 3:5))
#' norm(irregObject) # no extrapolation
#' norm(irregObject, fullDom = TRUE) # extrapolation (of second function)
#' 
#' # Multivariate
#' multiObject <- multiFunData(object, funData(argvals = 1:3, X = rbind(3:5, 6:8)))
#' norm(multiObject)
#' norm(multiObject, weight = c(2,1)) # with weight vector, giving more weight to the first element
setGeneric("norm", function(object,...) {
  standardGeneric("norm")})

#' Calculate the norm for univariate functional data
#'
#' @seealso \link{norm}
#'
#' @keywords internal
norm.funData <- function(object, squared, obs, method, weight)
{
  res <- weight*integrate(extractObs(object, obs)^2, method = method)
  
  if(!squared)
    res <- sqrt(res)
  
  return(res)
}

#' Calculate the norm for univariate functional data
#'
#' @seealso \link{norm} \linkS4class{funData}
#'
#' @keywords internal
setMethod("norm", signature = "funData",
          function(object, squared = TRUE, obs= 1:nObs(object), method = "trapezoidal", weight = 1){norm.funData(object,squared, obs, method, weight)})

#' Calculate the norm for multivariate functional data
#'
#' @seealso \link{norm} \linkS4class{multiFunData}
#'
#' @keywords internal
setMethod("norm", signature = "multiFunData",
          function(object, squared = TRUE, obs= 1:nObs(object), method = "trapezoidal", weight = rep(1, length(object)))
          {
            # univariate functions must be squared in any case!
            uniNorms <- mapply(norm, object = object, weight = weight, 
                               MoreArgs = list(squared = TRUE, obs = obs, method = method), SIMPLIFY = "array")
            
            # handle one observation separate, as rowSums does not work in that case...
            if(length(obs) == 1)
              res <- sum(uniNorms)
            else res <- rowSums(uniNorms)
            
            # should the norm be squared or not
            if(!squared) res <- sqrt(res)
            
            return(res)
          })


#' Calculate the norm for irregular functional data
#'
#' @seealso \link{norm}
#'
#' @keywords internal
norm.irregFunData <- function(object, squared, obs, method, weight, fullDom)
{
  object <- extractObs(object, obs)
  
  if(fullDom == TRUE) # extrapolate first
    object <- extrapolateIrreg(object)
  
  res <- weight*integrate(object^2, method = method, fullDom = FALSE)
  
  if(!squared)
    res <- sqrt(res)
  
  return(res)
}

#' Calculate the norm for irregular functional data
#'
#' @seealso \link{norm} \linkS4class{irregFunData}
#'
#' @keywords internal
setMethod("norm", signature = "irregFunData",
          function(object, squared = TRUE, obs= 1:nObs(object), method = "trapezoidal", weight = 1, fullDom = FALSE){
            norm.irregFunData(object, squared, obs, method, weight, fullDom)
          })


#### get/set ####

#' Extract and set slots from functional data objects
#' 
#' These functions can be used to extract and set the slots of \code{funData}, 
#' \code{irregFunData} and \code{multiFunData} objects.
#' 
#' Objects of class \code{funData} or \code{irregFunData} have two slots, 
#' \code{argvals} (for the x-values) and \code{X} (for the y-values for each 
#' observation). Using the \code{getArgvals} and \code{getX} methods for the 
#' classes \code{funData} and \code{irregFunData} is equivalent to accessing the
#' slots directly via \code{object@@argvals} and \code{object@@X}. Analogously, the
#' \code{setArgvals} and \code{setX} functions are equivalent to setting 
#' \code{object@@argvals} to \code{newArgvals} or \code{object@@X} to \code{newX}, 
#' respectively. The new values must hence have the same structure as the 
#' original ones. As an exception, for an object of class \code{funData}  the
#' number of new observations in \code{newX} may differ from the current. In
#' this case, the function throws a warning. 
#' 
#' Objects of class \code{multiFunData} are lists of several \code{funData} 
#' objects. The functions \code{getArgvals} and \code{getX} for \code{multiFunData}
#' objects therefore return a list of the same length as \code{object}, where 
#' each list element corresponds to the \code{argvals} or \code{X} slot of the 
#' univariate element. The \code{setArgvals} and \code{getArgvals} functions for 
#' \code{multiFunData} objects must be lists of the same length as 
#' \code{object}, where each list element corresponds to the new \code{argvals} or 
#' new \code{X} slot for the univariate elements.
#' 
#' For all classes classes, the set functions do not change the object, unless 
#' their result is assigned to \code{object} (see Examples).
#' 
#' @param object An object of class \code{funData}, \code{irregFunData} or 
#'   \code{multiFunData}.
#' @param newArgvals See Details.
#' @param newX See Details.
#'   
#' @return See Details.
#'   
#' @seealso \linkS4class{funData}, \linkS4class{irregFunData}, 
#'   \linkS4class{multiFunData}
#'   
#' @export getArgvals
#'   
#' @examples
#' ### Univariate
#' object <- funData(argvals = 1:5, X = rbind(1:5, 6:10))
#' object
#' 
#' # get-methods
#' getArgvals(object)
#' getX(object)
#' 
#' # set-methods
#' setArgvals(object, 0:4)
#' object # no change
#' object <- setArgvals(object, 0:4) # reassign the result to object
#' object # now, argvals is changed
#' \dontrun{object <- setArgvals(object, 1:4)} # wrong length
#' object <- setX(object, rbind(0:4, 5:9))
#' newObject <- setX(object, rbind(0:4, 5:9, 10:14)) # warning: now 3 observations (was 2 before)
#' \dontrun{object <- setX(object, rbind(1:4, 5:8))} # wrong length
#' 
#' ### Univariate (irregular)
#' irregObject <- irregFunData(argvals = list(1:5, 2:4), X=  list(2:6, 3:5))
#' irregObject
#' 
#' # get-methods
#' getArgvals(irregObject)
#' getX(irregObject)
#' 
#' newIrregObject <- setArgvals(irregObject, list(0:4, 1:3))
#' newIrregObject <- setX(irregObject, list(12:16, 13:15))
#' 
#' ### Multivariate
#' multiObject <- multiFunData(object, funData(argvals = 1:3, X = rbind(3:5, 6:8)))
#' multiObject
#' 
#' # get-methods
#' getArgvals(multiObject)
#' getX(multiObject)
#' 
#' # set-methods (for special cases see univariate version)
#' multiObject <- setArgvals(multiObject, list(5:1, 3:1))
#' multiObject <- setX(multiObject, list(rbind(5:1, 10:6), rbind(5:3, 8:6)))
setGeneric("getArgvals", function(object) {standardGeneric("getArgvals")})

#' Get argvals slot for funData objects
#'
#' @seealso \link{getArgvals}
#'
#' @keywords internal
setMethod("getArgvals", signature = "funData",
          function(object){object@argvals})

#' Get argvals slot for multiFunData objects
#'
#' @seealso \link{getArgvals}
#'
#' @keywords internal
setMethod("getArgvals", signature = "multiFunData",
          function(object){sapply(object, getArgvals)})

#' Get argvals slot for irregular functional data objects
#'
#' @seealso \link{getArgvals}
#'
#' @keywords internal
setMethod("getArgvals", signature = "irregFunData",
          function(object){object@argvals})


#'@rdname getArgvals
#'
#'@export getX
setGeneric("getX", function(object) {standardGeneric("getX")})

#' Get X slot for funData objects
#'
#' @seealso \link{getX}
#'
#' @keywords internal
setMethod("getX", signature = "funData",
          function(object){object@X})

#' Get X slot for multiFunData objects
#'
#' @seealso \link{getX}
#'
#' @keywords internal
setMethod("getX", signature = "multiFunData",
          function(object){sapply(object, getX, simplify = FALSE)})

#' Get X slot for irregular functional data objects
#'
#' @seealso \link{getX}
#'
#' @keywords internal
setMethod("getX", signature = "irregFunData",
          function(object){object@X})



#' @rdname getArgvals
#'
#' @export setArgvals
setGeneric("setArgvals", function(object, newArgvals) {standardGeneric("setArgvals")})

#' Set argvals slot for funData objects
#'
#' @seealso \link{setArgvals}
#'
#' @keywords internal
setMethod("setArgvals", signature = "funData",
          function(object, newArgvals){
            if(is.numeric(newArgvals))
              newArgvals <- list(newArgvals)
            object@argvals <- newArgvals; validObject(object); return(object)
          })

#' Set argvals slot for multiFunData objects
#'
#' @seealso \link{setArgvals}
#'
#' @keywords internal
setMethod("setArgvals", signature = "multiFunData",
          function(object, newArgvals){
            if(length(object)!=length(newArgvals))
              stop("setArgvals: multiFunData object and newArgvals must have the same length")
            multiFunData(mapply(setArgvals, object, newArgvals))
          })


#' Set argvals slot for irregular functional objects
#'
#' @seealso \link{setArgvals}
#'
#' @keywords internal
setMethod("setArgvals", signature = "irregFunData",
          function(object, newArgvals){
            if(length(object@argvals) != length(newArgvals))
              stop("setArgvals: newArgvals must be a list of the same length as the original argvals.")
            
            if(any(sapply(object@argvals, function(l){length(l)}) != sapply(newArgvals, function(l){length(l)})))
              stop("setArgvals: newArgvals must have the same structure as the original argvals.")
            
            object@argvals <- newArgvals
            
            return(object)
          })


#' @rdname getArgvals
#'
#' @export setX
setGeneric("setX", function(object, newX) {standardGeneric("setX")})

#' Set X slot for funData objects
#'
#' @seealso \link{setX}
#'
#' @keywords internal
setMethod("setX", signature = "funData",
          function(object, newX){
            if(nrow(object@X) != nrow(newX))
              warning("setX: Number of observations has changed")
            object@X <- newX; validObject(object); return(object)
          })

#' Set X slot for multiFunData objects
#'
#' @seealso \link{setX}
#'
#' @keywords internal
setMethod("setX", signature = "multiFunData",
          function(object, newX){
            if(length(object)!=length(newX))
              stop("setX: multiFunData object and newX must have the same length")
            
            if(diff(range(sapply(newX, function(x){dim(x)[1]}))) != 0)
              stop("setX: newX object must have the same number of observations in all elements!")
            
            multiFunData(mapply(setX, object, newX))
          })

#' Set X slot for irregular functional data objects
#'
#' @seealso \link{setX}
#'
#' @keywords internal
setMethod("setX", signature = "irregFunData",
          function(object, newX){
            if(length(object@X) != length(newX))
              stop("setX: newX must be a list of the same length as the original X.")
            
            if(any(sapply(object@X, function(l){length(l)}) != sapply(newX, function(l){length(l)})))
              stop("setX: newX must have the same structure as the original X.")
            
            object@X <- newX  
            
            return(object)
          })


#### flipFuns ####

#' Flip functional data objects
#' 
#' This function flips an object \code{newObject} of class \code{funData}, 
#' \code{irregFunData} or \code{multiFunData} with respect to a reference object
#' \code{refObject} of the same class (or of class \code{funData}, if 
#' \code{newObject} is irregular). This is particularly useful when dealing with
#' functional principal components, as they are only defined up to a sign 
#' change. For details, see below.
#' 
#' Functional principal component analysis is an important tool in functional 
#' data analysis. Just as eigenvectors, eigenfunctions (or functional principal 
#' components) are only defined up to a sign change. This may lead to 
#' difficulties in simulation studies or when bootstrapping pointwise confidence
#' bands, as in these cases one wants the estimates to have the same 
#' "orientation" as the true function (in simulation settings) or the 
#' non-bootstrapped estimate (when calculating bootstrap confidence bands). This
#' function allows to flip (i.e. multiply by \eqn{-1}{-1}) all observations in 
#' \code{newObject} that have a different orientation than their counterparts in
#' \code{refData}.
#' 
#' Technically, the function compares the distance between \code{newObject} and 
#' \code{refObject} \deqn{|||f_\mathrm{new} - f_\mathrm{ref}|||}{||| f_{new} - 
#' f_{ref}|||} and the distance between  \code{newObject} and 
#' \code{-1*refObject} \deqn{|||f_\mathrm{new} + f_\mathrm{ref}|||.}{||| f_{new} + 
#' f_{ref}|||.} If \code{newObject} is closer to \code{-1*refObject}, it is 
#' flipped, i.e. multiplied by -1.
#' 
#' The function is currently implemented only for functional data with one- and 
#' two-dimensional domains.
#' 
#' @param refObject An object of class \code{funData}, \code{irregFunData} or
#'   \code{multiFunData} that serves as reference. It must have the same number
#'   of observations as \code{newObject} or have only one observation. In this
#'   case, all observations in \code{newObject} are flipped with respect to this
#'   single observation.
#' @param newObject An object of class \code{funData}, \code{irregFunData} or
#'   \code{multiFunData} that is to be flipped with respect to \code{refObject}.
#' @param ... Further parameters passed to \code{\link{norm}}.
#'   
#' @return An object of the same class as \code{newData} with flipped 
#'   observations.
#'   
#' @seealso \linkS4class{funData}, \linkS4class{irregFunData},
#'   \linkS4class{multiFunData}, \link{Arith.funData}
#'   
#' @export flipFuns
#'   
#' @examples
#' 
#' ### Univariate
#' argvals <- seq(0,2*pi,0.01)
#' refData <- funData(argvals, rbind(sin(argvals))) # one observation as reference
#' newData <- funData(argvals, outer(sample(c(-1,1), 11, replace = TRUE) * seq(0.75, 1.25, by = 0.05),
#'                                sin(argvals)))
#' 
#' oldpar <- par(no.readonly = TRUE)
#' par(mfrow = c(1,2))
#' 
#' plot(newData, col = "grey", main = "Original data")
#' plot(refData, col = "red", lwd = 2, add = TRUE)
#' 
#' plot(flipFuns(refData, newData), col = "grey", main = "Flipped data")
#' plot(refData, col = "red", lwd = 2, add = TRUE)
#' 
#' ### Univariate (irregular)
#' ind <- replicate(11, sort(sample(1:length(argvals), sample(5:10,1)))) # sample observation points
#' argvalsIrreg <- lapply(ind, function(i){argvals[i]})
#' argvalsIrregAll <- unique(sort(unlist(argvalsIrreg)))
#'  # one observation as reference (fully observed)
#' refDataFull <- funData(argvals, rbind(sin(argvals)))
#'  # one observation as reference (irregularly observed)
#' refDataIrreg <- irregFunData(argvals = list(argvalsIrregAll), X = list(sin(argvalsIrregAll)))
#' newData <- irregFunData(argvals = argvalsIrreg, X = mapply(function(x, a, s){s * a * sin(x)},
#'      x = argvalsIrreg, a = seq(0.75, 1.25, by = 0.05), s = sample(c(-1,1), 11, replace = TRUE)))
#' 
#' plot(newData, col = "grey", main = "Original data (regular reference)")
#' plot(refDataFull, col = "red", lwd = 2, add = TRUE)
#' 
#' plot(flipFuns(refDataFull, newData), col = "grey", main = "Flipped data")
#' plot(refDataFull, col = "red", lwd = 2, add = TRUE)
#' 
#' plot(newData, col = "grey", main = "Original data (irregular reference)")
#' plot(refDataIrreg, col = "red", lwd = 2, add = TRUE)
#' 
#' plot(flipFuns(refDataIrreg, newData), col = "grey", main = "Flipped data")
#' plot(refDataIrreg, col = "red", lwd = 2, add = TRUE)
#' 
#' ### Multivariate
#' refData <- multiFunData(funData(argvals, rbind(sin(argvals))), # one observation as reference
#'                         funData(argvals, rbind(cos(argvals)))) 
#' sig <- sample(c(-1,1), 11, replace = TRUE) 
#' newData <- multiFunData(funData(argvals, outer(sig * seq(0.75, 1.25, by = 0.05), sin(argvals))),
#'                         funData(argvals, outer(sig * seq(0.75, 1.25, by = 0.05), cos(argvals))))
#'                         
#' par(mfrow = c(2,2))
#' 
#' plot(newData[[1]], col = topo.colors(11), main = "Original data")
#' plot(refData[[1]], col = "red", lwd = 2, add = TRUE)
#' 
#' plot(newData[[2]], col = topo.colors(11), main = "Original data")
#' plot(refData[[2]], col = "red", lwd = 2, add = TRUE)
#' 
#' plot(flipFuns(refData, newData)[[1]], col = topo.colors(11), main = "Flipped data")
#' plot(refData[[1]], col = "red", lwd = 2, add = TRUE)
#' 
#' plot(flipFuns(refData, newData)[[2]], col = topo.colors(11), main = "Flipped data")
#' plot(refData[[2]], col = "red", lwd = 2, add = TRUE)
#' 
#' par(oldpar)
setGeneric("flipFuns", function(refObject, newObject, ...) {standardGeneric("flipFuns")})


#' Flip univariate functional data
#'
#' @seealso \link{flipFuns}
#'
#' @keywords internal
setMethod("flipFuns", signature = c("funData", "funData"),
          function(refObject, newObject){
            
            if(dimSupp(newObject) > 2)
              stop("flipFuns: Function is only implemented for data of dimension <= 2")
            
            if( (! nObs(refObject) == nObs(newObject)) & (! nObs(refObject) == 1))
              stop("flipFuns: Functions must have the same number of observations or use a single function as reference.")
            
            if(dimSupp(refObject) != dimSupp(newObject))
              stop("flipFuns: Functions must have the dimension.")
            
            if(!isTRUE(all.equal(refObject@argvals, newObject@argvals)))
              stop("flipFuns: Functions must be defined on the same domain.")
            
            # calculate signs: flip if newObject is closer to -refObject than to refObject
            sig <- ifelse(norm(newObject + refObject) < norm(newObject - refObject), -1, 1)
            
            # flip functions
            newObject@X <- sig*newObject@X
            
            return(newObject)
          })


#' Flip multivariate functional data
#'
#' @seealso \link{flipFuns}
#'
#' @keywords internal
setMethod("flipFuns", signature = signature("multiFunData", "multiFunData"),
          function(refObject, newObject){
            if(length(refObject)!=length(newObject))
              stop("flipFuns: multiFunData objects must have the same length")
            
            if( (! nObs(refObject) == nObs(newObject)) & (! nObs(refObject) == 1))
              stop("flipFuns: Functions must have the same number of observations or use a single function as reference.")
            
            if(any(dimSupp(refObject) != dimSupp(newObject)))
              stop("flipFuns: Functions must have the dimension.")
            
            if(!isTRUE(all.equal(getArgvals(refObject), getArgvals(newObject))))
              stop("flipFuns: Functions must be defined on the same domain.")
            
            # calculate signs: flip if newObject is closer to -refObject than to refObject
            sig <- ifelse(norm(newObject + refObject) < norm(newObject - refObject), -1, 1)
            
            for(j in 1:length(newObject))
              newObject[[j]]@X <- sig*newObject[[j]]@X
            
            return(newObject)
          })


#' Flip irregular functional data - funData as reference
#'
#' @seealso \link{flipFuns}
#'
#' @keywords internal
setMethod("flipFuns", signature = c("funData", "irregFunData"),
          function(refObject, newObject,...){
            
            if(any(c(dimSupp(refObject), dimSupp(newObject)) > 1))
              stop("flipFuns: Function is only implemented for irregular data with one-dimensional support")
            
            if( (! nObs(refObject) == nObs(newObject)) & (! nObs(refObject) == 1))
              stop("flipFuns: Functions must have the same number of observations or use a single function as reference.")
            
            if(!all(unlist(newObject@argvals) %in% unlist(refObject@argvals)))
              stop("flipFuns: Irregular functions must be defined on a sub-domain of the reference function(s).")
            
            # calculate signs: flip if newObject is closer to -refObject than to refObject
            sig <- ifelse(norm(newObject + refObject,...) < norm(newObject - refObject,...), -1, 1)
            
            # flip functions
            newObject@X <- mapply(function(s,y){s*y}, s = sig, y = newObject@X)
            
            return(newObject)
          })


#' Flip irregular functional data - irregFunData as reference
#'
#' @seealso \link{flipFuns}
#'
#' @keywords internal
setMethod("flipFuns", signature = c("irregFunData", "irregFunData"),
          function(refObject, newObject,...){
            
            #    if(any(dimSupp(refObject), dimSupp(newObject)) > 1)
            #      stop("flipFuns: Function is only implemented for irregular data with one-dimensional support")
            
            if( (! nObs(refObject) == nObs(newObject)) & (! nObs(refObject) == 1))
              stop("flipFuns: Functions must have the same number of observations or use a single function as reference.")
            
            if(!all(unlist(newObject@argvals) %in% unlist(refObject@argvals)))
              stop("flipFuns: New functions must be defined on a sub-domain of the reference function(s).")
            
            # calculate signs: flip if newObject is closer to -refObject than to refObject
            sig <- ifelse(norm(newObject + refObject,...) < norm(newObject - refObject,...), -1, 1)
            
            # flip functions
            newObject@X <- mapply(function(s,y){s*y}, s = sig, y = newObject@X)
            
            return(newObject)
          })


#### meanFunction ####

#' Mean for functional data
#' 
#' This function calculates the pointwise mean function for objects of class 
#' \code{funData}, \code{irregFunData} or \code{multiFunData}.
#' 
#' @section Warning:
#' If \code{object} is of class \code{irregFunData}, the option \code{na.rm =
#' TRUE} is not implemented and throws an error. If \code{na.rm = FALSE}, the
#' functions must be observed on the same domain.
#' 
#' @param object An object of class \code{funData}, \code{irregFunData} or 
#'   \code{multiFunData}.
#' @param na.rm Logical. If \code{TRUE}, NA values are removed before computing 
#'   the mean. Defaults to \code{FALSE}.
#'   
#' @return An object of the same class as \code{object} with one observation 
#'   that corresponds to the pointwise mean function of the functions in 
#'   \code{object}.
#'   
#' @seealso \linkS4class{funData}, \linkS4class{irregFunData}, 
#'   \linkS4class{multiFunData}, \link{Arith.funData}
#'   
#' @export meanFunction
#'   
#' @examples
#' ### Univariate (one-dimensional support)
#' x <- seq(0, 2*pi, 0.01)
#' f1 <- funData(x, outer(seq(0.75, 1.25, 0.05), sin(x)))
#' 
#' plot(f1)
#' plot(meanFunction(f1), col = 1, lwd = 2, add = TRUE)
#' 
#' ### Univariate (two-dimensional support)
#' f2 <- funData(list(1:5, 1:3), array(rep(1:5,each = 11, times = 3), dim = c(11,5,3)))
#' all.equal(extractObs(f2,1), meanFunction(f2)) # f2 has 11 identical observations
#' 
#' ### Multivariate
#' m1 <- multiFunData(f1,f2)
#' all.equal(extractObs(m1, obs = 6), meanFunction(m1)) # observation 6 equals the pointwise mean
#' 
#' ### Irregular
#' i1 <- irregFunData(argvals = list(1:3,1:3,1:3), X = list(1:3,2:4,3:5))
#' all.equal(meanFunction(i1), extractObs(i1, obs = 2))
#' # don't run: functions are not defined on the same domain
#' \dontrun{meanFunction(irregFunData(argvals = list(1:3,1:5), X = list(1:3,1:5))) }
setGeneric("meanFunction", function(object, na.rm = FALSE) {standardGeneric("meanFunction")})


#' Mean for functional data
#'
#' @seealso \link{meanFunction}
#'
#' @keywords internal
setMethod("meanFunction", signature = c("funData", "ANY"),
          function(object, na.rm){
            meanX <- colMeans(object@X, na.rm = na.rm, dims = 1) 
            
            # resize to array with one observation
            if(dimSupp(object) == 1)
              dim(meanX) <- length(meanX)
            dim(meanX) <- c(1, dim(meanX))
            
            funData(argvals = object@argvals, X = meanX)
          })

#' Mean for multivariate functional data
#'
#' @seealso \link{meanFunction}
#'
#' @keywords internal
setMethod("meanFunction", signature = c("multiFunData", "ANY"),
          function(object, na.rm){
            univMean <- selectMethod("meanFunction", "funData")
            multiFunData(lapply(object, univMean, na.rm))
          })

#' Mean for irregular functional data
#'
#' @seealso \link{meanFunction}
#'
#' @keywords internal
setMethod("meanFunction", signature = c("irregFunData", "ANY"),
          function(object, na.rm){
            if(na.rm == TRUE)
              stop("Option na.rm = TRUE is not implemented for mean functions of irregular data.")
            
            if(!all(sapply(object@argvals[-1], function(x){isTRUE(all.equal(object@argvals[[1]], x))})))
              stop("Mean function defined only for irregular functional data objects on the same domain.")
            
            irregFunData(object@argvals[1], list(sapply(object@X, mean)))
          })


#### tensorProduct ####

#' Tensor product for univariate functions on one-dimensional domains
#' 
#' This function calculates the tensor product function for objects of class 
#' \code{funData} defined on one-dimensional domains.
#' 
#' @section Warning: The function is only implemented for up to three functions 
#'   f1, f2, f3 on one-dimensional domains.
#'   
#' @param ... Two or three objects of class \code{funData}, that must be defined
#'   on a one-dimensional domain, each.
#'   
#' @return An object of class as \code{funData} that corresponds to the tensor
#'   product of the input functions.
#'   
#' @seealso \linkS4class{funData}
#'   
#' @export tensorProduct
#'   
#' @examples
#' 
#' ### Tensor product of two functional data objects
#' x <- seq(0, 2*pi, 0.1)
#' f1 <- funData(x, outer(seq(0.75, 1.25, 0.1), sin(x)))
#' y <- seq(-pi, pi, 0.1)
#' f2 <- funData(y, outer(seq(0.25, 0.75, 0.1), sin(y)))
#' 
#' plot(f1, main = "f1")
#' plot(f2, main = "f2")
#' 
#' tP <- tensorProduct(f1, f2)
#' dimSupp(tP)
#' plot(tP, obs = 1)
#' 
#' ### Tensor product of three functional data objects
#' z <- seq(-1, 1, 0.05)
#' f3 <- funData(z, outer(seq(0.75, 1.25, 0.1), z^2))
#' 
#' plot(f1, main = "f1")
#' plot(f2, main = "f2")
#' plot(f3, main = "f3")
#' 
#' tP2 <- tensorProduct(f1, f2, f3)
#' dimSupp(tP2)
setGeneric("tensorProduct", function(...) {standardGeneric("tensorProduct")})


#' Tensor product for functional data
#'
#' @seealso \link{meanFunction}
#'
#' @keywords internal
setMethod("tensorProduct", signature = c("funData"),
          function(...){
          
            l <- list(...) # combine all arguments in a list
          
            if(length(l) != 2 & length(l) != 3)
              stop("tensorProduct currently accepts only 2 or 3 arguments.")
            
            if(any(sapply(l, dimSupp) != 1))
              stop("tensorProduct is defined only for funData objects on one-dimensional domains!")
            
            g <- expand.grid(lapply(l, function(f){1:nObs(f)}))
            
            if(length(l) == 2)
            {
              res <- sapply(1:dim(g)[1], function(i){l[[1]]@X[g[i,1],] %o% l[[2]]@X[g[i,2],] }, simplify = "array")
              res <- aperm(res, c(3,1,2))
            } 
            else # length(l) = 3
            {
              res <- sapply(1:dim(g)[1], function(i){l[[1]]@X[g[i,1],] %o% l[[2]]@X[g[i,2],] %o% l[[3]]@X[g[i,3],]}, simplify = "array")
              res <- aperm(res, c(4,1,2,3))
            } 
            
            resFun <- funData(argvals = lapply(l, function(f){f@argvals[[1]]}), X = res)
            
            return(resFun)
})