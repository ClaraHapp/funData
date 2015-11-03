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
  cat("Functional data object with", nObs(x) ,"observations of", dimSupp(x) ,"- dimensional support\nxVal:\n")
  
  for(i in 1:dimSupp(x))
  {
    cat("\t")
    if(length(x@xVal[[i]]) > 5)
      cat(x@xVal[[i]][1], x@xVal[[i]][2], "...", x@xVal[[i]][length(x@xVal[[i]])])
    else
      cat(x@xVal[[i]])
    cat("\t\t(", length(x@xVal[[i]]), " sampling points)\n", sep = "")
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
  
  cat("xVal:\n\tValues in ", paste(round(range(x@xVal),3), collapse = " ... "), ".\n", sep = "")
  
  cat("X:\n\tValues in ", paste(round(range(x@X),3), collapse = " ... "),".\n", sep = "")
  
  cat("Total:\n\t", length(unlist(x@xVal)), " observations on " , length(unique(unlist(x@xVal))), " different x-values (",
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
#' object1 <- funData(xVal = 1:5, X = rbind(1:5, 6:10))
#' dimSupp(object1)
#'
#' # Univariate (two-dimensional)
#' object2 <- funData(xVal = list(1:10, 1:5), X = array(rnorm(100), dim = c(2,10,5)))
#' dimSupp(object2)
#' 
#' # Univariate (irregular)
#' irregObject <- irregFunData(xVal = list(1:5, 2:4), X = list(2:6, 3:5))
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
          function(object){length(object@xVal)})


#' dimSupp for multiFunData objects
#'
#' @keywords internal
setMethod("dimSupp", signature = "multiFunData",
          function(object){sapply(object, dimSupp, simplify = TRUE)})

#' dimSupp for irregular functional data objects
#'
#' @keywords internal
setMethod("dimSupp", signature = "irregFunData",
          function(object){length(dim(as.array(object@xVal[[1]])))})

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
#' @param xlab,ylab The titles for x- and y-axis. Defaults to "xVal" for the
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
#' xVal <- seq(0,2*pi,0.01)
#' object <- funData(xVal,
#'                    outer(seq(0.75, 1.25, length.out = 11), sin(xVal)))
#'
#' plot(object, main = "One-dimensional functional data")
#'
#' # Two-dimensional
#' X <- array(0, dim = c(2, length(xVal), length(xVal)))
#' X[1,,] <- outer(xVal, xVal, function(x,y){sin((x-pi)^2 + (y-pi)^2)})
#' X[2,,] <- outer(xVal, xVal, function(x,y){sin(2*x*pi)*cos(2*y*pi)})
#' object2D <- funData(list(xVal, xVal), X)
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
                         col =NULL, xlab = "xVal", ylab = "", legend = TRUE,
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
        matplot(x = x@xVal[[1]], y = zoo::na.approx(t(x@X[obs,, drop = FALSE])), type = "l", lty = lty,  lwd = lwd, col = col, xlab = xlab, ylab = ylab, ...)
        
        add = TRUE # add the standard plot
      }
      else
        warning("Package zoo needed for interpolating missing values in plot for funData. Ignoring plotNA = TRUE.")
    }
    
    matplot(x = x@xVal[[1]], y = t(x@X[obs,, drop = FALSE]), type = type, lty = lty,  lwd = lwd, col = col, xlab = xlab, ylab = ylab, add = add, ...)
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
      fields::image.plot(x = x@xVal[[1]], y = x@xVal[[2]], z = x@X[obs, ,], lty = lty, xlab = xlab, ylab = ylab, col = col, ...)
    }
    else
    {
      image(x = x@xVal[[1]], y = x@xVal[[2]], z = x@X[obs, ,], lty = lty, xlab = xlab, ylab = ylab, col = col, ...)
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
#' @section Warning:
#' The function is currently implemented only for functional data with one- and
#' two-dimensional domains.
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
#' @param add Logical. If \code{TRUE}, add to current plot (only for
#'   one-dimensional functions). Defaults to \code{FALSE}.
#' @param main A string vector, giving the title of the plot. Can have the same
#'   length as \code{dim} (different titles for each dimension) or length
#'   \code{1} (one title for all dimensions). Defaults to \code{NULL}.
#' @param ... Additional arguments to \code{plot}.
#'
#' @seealso \linkS4class{funData}, \linkS4class{multiFunData},
#'   \code{\link{plot.funData}}
#'
#' @examples
#' oldpar <- par(no.readonly = TRUE)
#' xVal <- seq(0, 2*pi, 0.1)
#'
#' # One-dimensional elements
#' object1 <- funData(xVal, outer(seq(0.75, 1.25, length.out = 11), sin(xVal)))
#' object2 <- funData(xVal, outer(seq(0.75, 1.25, length.out = 11), cos(xVal)))
#'
#' multiObject <- multiFunData(object1, object2)
#' plot(multiObject, main = c("1st element", "2nd element")) # different titles
#' plot(multiObject, main = "Multivariate Functional Data") # one title for all
#'
#' # Mixed-dimensional elements
#' X <- array(0, dim = c(11, length(xVal), length(xVal)))
#' X[1,,] <- outer(xVal, xVal, function(x,y){sin((x-pi)^2 + (y-pi)^2)})
#' object2D <- funData(list(xVal, xVal), X)
#'
#' multiObject <- multiFunData(object1, object2D)
#' plot(multiObject, main = c("1st element", "2nd element"), obs = 1) # different titles
#' plot(multiObject, main = "Multivariate Functional Data", obs = 1) # one title for all
#'
#' \dontrun{plot(multiObject, main = c("1st element", "2nd element")) # must specify obs!}
#' \dontrun{plot(multiObject, main = "Multivariate Functional Data") # must specify obs!}
#'
#' par(oldpar)
plot.multiFunData <- function(x, y, obs = 1:nObs(x), dim = 1:length(x), par.plot = NULL, add = FALSE, main = NULL, ...){
  
  if(add == FALSE)
  {
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
    
  }
  
  if(!is.null(main) & (length(main) == 1))
    main <- rep(main, length(dim))
  
  
  # plot the univariate functions
  for(i in dim)
    plot(x[[i]], obs = obs, add = add, main = main[i], ...)
  
  
  # if no par.plot specified: reset graphics parameters
  if(add == FALSE & is.null(par.plot))
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
#' @param xlab,ylab The titles for x- and y-axis. Defaults to "xVal" for the 
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
#' xVal <- seq(0,2*pi,0.01)
#' ind <- replicate(5, sort(sample(1:length(xVal), sample(5:10,1))))
#' object <- irregFunData(xVal = lapply(ind, function(i){xVal[i]}),
#'                   X = lapply(ind, function(i){sample(1:10, 1)/10*xVal[i]^2}))
#' 
#' plot(object, main = "Irregular functional data")
#' 
#' par(oldpar)
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

#' Arithmetics of functional data objects
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
#' xVal <- seq(0, 2*pi, 0.01)
#' object1 <- funData(xVal, outer(seq(0.75, 1.25, by = 0.05), sin(xVal)))
#' object2 <- funData(xVal, outer(seq(0.75, 1.25, by = 0.05), cos(xVal)))
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
#' ind <- replicate(11, sort(sample(1:length(xVal), sample(5:10, 1))))
#' i1 <- irregFunData(
#'    xVal = lapply(1:11, function(i, ind, x){x[ind[[i]]]}, ind = ind, x = object1@@xVal[[1]]),
#'    X = lapply(1:11, function(i, ind, y){y[i, ind[[i]]]}, ind = ind, y = object1@@X))
#' i2 <- irregFunData(
#'    xVal = lapply(1:11, function(i, ind, x){x[ind[[i]]]}, ind = ind, x = object2@@xVal[[1]]),
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
#' @importFrom plyr aaply
setMethod("Arith", signature = c(e1 = "funData", e2 = "funData"),
          function(e1, e2){
            if(!all.equal(e1@xVal, e2@xVal))
              stop("Arithmetics: Functions must be defined on the same domain!")
            
            if(nObs(e1) == nObs(e2))
              resX <- methods::callGeneric(e1@X, e2@X)
            else
            {
              if(all(c(nObs(e1), nObs(e2)) > 1))
                stop("Arithmethics: nObs of funData objects is neigther equal nor one.")
              
              f <- function(x,y){methods::callGeneric(x,y)} # helper function (callGeneric not applicable in lapply)
              
              # if one funData object has only a single observation: apply to all of the other object
              if(nObs(e1) == 1 & nObs(e2) > 1) 
                resX <- plyr::aaply(e2@X, 1, function(x){ f(array(e1@X, dim = dim(e1@X)[-1]), x) })
              
              if(nObs(e2) == 1 & nObs(e1) > 1) 
                resX <- plyr::aaply(e1@X, 1, function(x){ f(x, array(e2@X, dim = dim(e2@X)[-1])) })      
            }
            
            funData(xVal = e1@xVal, X = resX)
          })

#' @rdname Arith.funData
setMethod("Arith", signature = c(e1 = "funData", e2 = "numeric"),
          function(e1, e2) {
            funData(xVal = e1@xVal, X = methods::callGeneric(e1@X, e2))
          })

#' @rdname Arith.funData
setMethod("Arith", signature = c(e1 = "numeric", e2 = "funData"),
          function(e1, e2) {
            funData(xVal = e2@xVal, X = methods::callGeneric(e1, e2@X))
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
            irregFunData(xVal = e1@xVal, X = lapply(e1@X, function(x){f(x,e2)}))
          })

#' @rdname Arith.funData
setMethod("Arith", signature = c(e1 = "numeric", e2 = "irregFunData"),
          function(e1, e2) {
            f <- function(x,y){methods::callGeneric(x,y)} # helper function (callGeneric not applicable in lapply)
            irregFunData(xVal = e2@xVal, X = lapply(e2@X, function(x){f(e1,x)}))
          })

#' @rdname Arith.funData
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

#' @rdname Arith.funData
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

#' @rdname Arith.funData
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
#' object <- funData(xVal = 1:5, X = rbind(1:5, 6:10))
#' nObs(object)
#' 
#' # Univariate (irregular)
#' irregObject <- irregFunData(xVal = list(1:5, 2:4), X = list(2:6, 3:5))
#' nObs(irregObject)
#'
#' # Multivariate
#' multiObject <- multiFunData(object, funData(xVal = 1:3, X = rbind(3:5, 6:8)))
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
#' object1 <- funData(xVal = 1:5, X = rbind(1:5, 6:10))
#' nObsPoints(object1)
#' 
#' # Univariate (two-dimensional)
#' object2 <- funData(xVal = list(1:5, 1:6), X = array(1:60, dim = c(2, 5, 6)))
#' nObsPoints(object2)
#' 
#' # Multivariate
#' multiObject <- multiFunData(object1, object2)
#' nObsPoints(multiObject)
#' 
#' # Univariate (irregular)
#' irregObject <- irregFunData(xVal = list(1:5, 2:4), X = list(2:6, 3:5))
#' nObsPoints(irregObject)
setGeneric("nObsPoints", function(object) {standardGeneric("nObsPoints")})

#' nObsPoints for funData objects
#'
#' @keywords internal
setMethod("nObsPoints", signature = "funData", 
          function(object){sapply(object@xVal, length)})

#' nObsPoints for multiFunData objects
#'
#' @keywords internal
setMethod("nObsPoints", signature = "multiFunData", 
          function(object){lapply(object, nObsPoints)})

#' nObsPoints for irregular functional data objects
#'
#' @keywords internal
setMethod("nObsPoints", signature = "irregFunData", 
          function(object){sapply(object@xVal, length)})

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
#' @param xVal The part of the domain to be extracted (default: the whole domain
#'   object@@xVal). Must be a list or a numeric vector (only for one-dimensional
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
#' object1 <- funData(xVal = 1:5, X = rbind(1:5, 6:10))
#' extractObs(object1, obs = 1)
#' extractObs(object1, xVal = 1:3)
#' extractObs(object1, xVal = list(1:3)) # the same as the statement before
#' 
#' # Univariate - two-dimensional domains
#' object2 <- funData(xVal = list(1:5, 1:6), X = array(1:60, dim = c(2, 5, 6)))
#' extractObs(object2, obs = 1)
#' extractObs(object2, xVal = list(1:3, c(2,4,6))) # xVals must be supplied as list
#' 
#' # Univariate - irregular
#' irregObject <- irregFunData(xVal = list(1:5, 2:4), X = list(2:6, 3:5))
#' extractObs(irregObject, obs = 2)
#' extractObs(irregObject, xVal = 1:3)
#' extractObs(irregObject, xVal = c(1,5)) # throws a warning, as second function has no observations
#' 
#' # Multivariate
#' multiObject <- multiFunData(object1, object2)
#' extractObs(multiObject, obs = 2)
#' extractObs(multiObject, xVal = list(1:3, list(1:3, c(2,4,6))))
setGeneric("extractObs", function(object, obs = 1:nObs(object), xVal= getxVal(object)) {
  standardGeneric("extractObs")
})

#' extractObs for funData objects
#'
#' @keywords internal
setMethod("extractObs", signature = signature("funData", "ANY", "ANY"),
          function(object, obs, xVal){
            
            if(dimSupp(object) > 3)
              stop("extracting observations is not implemented yet for functional data of dimension > 3")
            
            if(!is.numeric(obs))
              stop("Supply observations as numeric vector")
            
            if(!all((1:nObs(object))[obs] %in% 1:nObs(object)))
              stop("Trying to extract observations that do not exist!")
            
            if(!is.list(xVal))
            {
              if(dimSupp(object) == 1 & is.numeric(xVal))
                xVal = list(xVal)
              else
                stop("Supply xVals for exstracted observations either as list or as numeric vector (only if support is one-dimensional)")
            }
            
            if(!all(unlist(mapply(function(x,y){x%in%y}, xVal,object@xVal))))
              stop("Trying to extract x-values that do not exist!")
            
            if(dimSupp(object) == 1)
              return(funData(xVal, object@X[obs,object@xVal[[1]] %in% xVal[[1]], drop = FALSE]))
            
            if(dimSupp(object) == 2)
              return(funData(xVal, object@X[obs,object@xVal[[1]] %in% xVal[[1]],object@xVal[[2]] %in% xVal[[2]], drop = FALSE]))
          })

#' extractObs for multiFunData objects
#'
#' @keywords internal
setMethod("extractObs", signature = signature("multiFunData", "ANY", "ANY"), def = function(object, obs, xVal){
  if(!missing(xVal) & !is.list(xVal))
    stop("extractObs for multiFunData: xVal must be supplied as list (or missing).")
  if(is.list(obs))
    res <-   multiFunData(mapply(extractObs, object = object, obs = obs, xVal = xVal))
  else
    res <-  multiFunData(mapply(extractObs, object = object, xVal = xVal, MoreArgs = list(obs = obs)))
})

#' extractObs for irregular functional data
#' 
#' @keywords internal
setMethod("extractObs", signature = signature("irregFunData", "ANY", "ANY"),
function(object, obs, xVal){
  #  if(dimSupp(object) > 1)
  #    stop("Extracting observations is not implemented yet for functional data of dimension > 1")
  
  if(!is.numeric(obs))
    stop("Supply observations as numeric vector")
  
  if(!all((1:nObs(object))[obs] %in% 1:nObs(object)))
    stop("Trying to extract observations that do not exist!")
  
  if(!is.list(xVal))
  {
    if(is.numeric(xVal))
      xVal = list(xVal)
    else
      stop("Supply xVals for extracted observations either as list or as numeric vector")
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
#'   data with one- and two-dimensional domains.
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
#' object <- funData(xVal = 1:5, X = rbind(1:5, 6:10))
#' integrate(object)
#' 
#' # Univariate (irregular)
#' irregObject <-irregFunData(xVal = list(1:5, 2:4), X = list(2:6, 3:5))
#' integrate(irregObject) # fullDom = FALSE
#' integrate(irregObject, fullDom = TRUE)
#' 
#' # Multivariate
#' multiObject <- multiFunData(object, funData(xVal = 1:3, X = rbind(3:5, 6:8)))
#' integrate(multiObject)
setGeneric("integrate", function(object, ...) {standardGeneric("integrate")})

# integration weights (internal functions)

#' Calculate weights for numerical integration
#'
#' This function calculates the weights for numerical integration
#'
#' @param xVal A numeric vector of x-Values
#' @param method A character string, giving the numerical integration method to use (default is \code{trapezoidal}, alternatively use \code{midpoint})
#'
#' @return A vector of integration weights
#'
#' @seealso \ref{integrate}
#'
#' @keywords internal
.intWeights <- function(xVal, method = "trapezoidal")
{
  if(method == "trapezoidal" & (length(xVal) < 3))
  {
    method <- "midpoint"
    warning("Trapezoidal quadrature is not applicable for functions with < 3 observation points. 'method' changed to 'midpoint'.")
  } 
  
  ret <- switch(method,
                midpoint = c(0,diff(xVal)),
                trapezoidal = {D <- length(xVal)
                               1/2*c(xVal[2] - xVal[1], xVal[3:D] -xVal[1:(D-2)], xVal[D] - xVal[D-1])},
                stop("function intWeights: choose either midpoint or trapezoidal quadrature rule"))
  
  return(ret)
}

#' Integrate method for funData objects
#'
#' @seealso \link{integrate} \linkS4class{funData}
#'
#' @keywords internal
setMethod("integrate", signature = "funData",
          function(object, method = "trapezoidal"){
            if(dimSupp(object) > 2)
              stop("Integration is not yet defined for functional data objects with dim > 2")
            
            if(dimSupp(object) == 1)
              res <- object@X %*% .intWeights(object@xVal[[1]],method)
            
            if(dimSupp(object) == 2)
              res <- apply( object@X , 1, function(x){t(.intWeights(object@xVal[[1]])) %*% x %*% .intWeights(object@xVal[[2]])})
            
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
                          x = object@xVal, y = object@X, MoreArgs = list(method = method)))
          })


#### extrapolateIrreg ####

#' Extrapolate irregular functional data to a given domain
#' 
#' This function extrapolates an \code{irregFunData} object to a given domain.
#' If only one point is observed, the function is extrapolated as a constant; in
#' all other cases it is extrapolated linearly.
#' 
#' @keywords internal
extrapolateIrreg <- function(object, rangex = range(object@xVal))
{  
  for(i in 1:nObs(object))
  {
    e <- .extrapolate(object@xVal[[i]], object@X[[i]], rangex)
    object@xVal[[i]] <- e$x
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
#' \link{integrate} for details. \item \code{fullDom}: Logical. If \code{object}
#' is of class \linkS4class{irregFunData} and \code{fullDom = TRUE}, all functions are extrapolated
#' to the same domain. Defaults to \code{FALSE}. See \link{integrate} for details. }
#' 
#' @section Warning: The function is currently implemented only for functional
#'   data with one- and two-dimensional domains.
#'   
#' @param object An object of class \code{funData}, \code{irregFunData} or \code{multiFunData}.
#' @param ... Further parameters (see Details).
#'   
#' @return A numeric vector representing the norm of each observation.
#'   
#' @seealso \linkS4class{funData}, \linkS4class{irregFunData}, \linkS4class{multiFunData},
#'   \code{\link{integrate}}
#'   
#' @export norm
#'   
#' @examples
#' # Univariate
#' object <- funData(xVal = 1:5, X = rbind(1:5, 6:10))
#' norm(object)
#' 
#' # Univariate (irregular)
#' irregObject <- irregFunData(xVal = list(1:5, 2:4), X = list(2:6, 3:5))
#' norm(irregObject) # no extrapolation
#' norm(irregObject, fullDom = TRUE) # extrapolation (of second function)
#' 
#' # Multivariate
#' multiObject <- multiFunData(object, funData(xVal = 1:3, X = rbind(3:5, 6:8)))
#' norm(multiObject)
setGeneric("norm", function(object,...) {
  standardGeneric("norm")})

#' Calculate the norm for univariate functional data
#'
#' @seealso \link{norm}
#'
#' @keywords internal
norm.funData <- function(object, squared, obs, method)
{
  res <- integrate(extractObs(object, obs)^2, method = method)
  
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
          function(object, squared = TRUE, obs= 1:nObs(object), method = "trapezoidal"){norm.funData(object,squared, obs, method)})

#' Calculate the norm for multivariate functional data
#'
#' @seealso \link{norm} \linkS4class{multiFunData}
#'
#' @keywords internal
setMethod("norm", signature = "multiFunData",
          function(object, squared = TRUE, obs= 1:nObs(object), method = "trapezoidal")
          {
            # univariate functions must be squared in any case!
            uniNorms <- sapply(object, norm,  squared = TRUE, obs, method, simplify = "array")
            
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

#' Calculate the norm for irregular functional data
#'
#' @seealso \link{norm} \linkS4class{irregFunData}
#'
#' @keywords internal
setMethod("norm", signature = "irregFunData",
          function(object, squared = TRUE, obs= 1:nObs(object), method = "trapezoidal", fullDom = FALSE){
            norm.irregFunData(object, squared, obs, method, fullDom)
          })


#### get/set ####

#' Extract and set slots from functional data objects
#' 
#' These functions can be used to extract and set the slots of \code{funData}, 
#' \code{irregFunData} and \code{multiFunData} objects.
#' 
#' Objects of class \code{funData} or \code{irregFunData} have two slots, 
#' \code{xVal} (for the x-values) and \code{X} (for the y-values for each 
#' observation). Using the \code{getxVal} and \code{getX} methods for the 
#' classes \code{funData} and \code{irregFunData} is equivalent to accessing the
#' slots directly via \code{object@@xVal} and \code{object@@X}. Analogously, the
#' \code{setxVal} and \code{setX} functions are equivalent to setting 
#' \code{object@@xVal} to \code{newxVal} or \code{object@@X} to \code{newX}, 
#' respectively. The new values must hence have the same structure as the 
#' original ones. As an exception, for an object of class \code{funData}  the
#' number of new observations in \code{newX} may differ from the current. In
#' this case, the function throws a warning. 
#' 
#' Objects of class \code{multiFunData} are lists of several \code{funData} 
#' objects. The functions \code{getxVal} and \code{getX} for \code{multiFunData}
#' objects therefore return a list of the same length as \code{object}, where 
#' each list element corresponds to the \code{xVal} or \code{X} slot of the 
#' univariate element. The \code{setxVal} and \code{getxVal} functions for 
#' \code{multiFunData} objects must be lists of the same length as 
#' \code{object}, where each list element corresponds to the new \code{xVal} or 
#' new \code{X} slot for the univariate elements.
#' 
#' For all classes classes, the set functions do not change the object, unless 
#' their result is assigned to \code{object} (see Examples).
#' 
#' @param object An object of class \code{funData}, \code{irregFunData} or 
#'   \code{multiFunData}.
#' @param newxVal See Details.
#' @param newX See Details.
#'   
#' @return See Details.
#'   
#' @seealso \linkS4class{funData}, \linkS4class{irregFunData}, 
#'   \linkS4class{multiFunData}
#'   
#' @export getxVal
#'   
#' @examples
#' ### Univariate
#' object <- funData(xVal = 1:5, X = rbind(1:5, 6:10))
#' object
#' 
#' # get-methods
#' getxVal(object)
#' getX(object)
#' 
#' # set-methods
#' setxVal(object, 0:4)
#' object # no change
#' object <- setxVal(object, 0:4) # reassign the result to object
#' object # now, xVal is changed
#' \dontrun{object <- setxVal(object, 1:4)} # wrong length
#' object <- setX(object, rbind(0:4, 5:9))
#' newObject <- setX(object, rbind(0:4, 5:9, 10:14)) # warning: now 3 observations (was 2 before)
#' \dontrun{object <- setX(object, rbind(1:4, 5:8))} # wrong length
#' 
#' ### Univariate (irregular)
#' irregObject <- irregFunData(xVal = list(1:5, 2:4), X=  list(2:6, 3:5))
#' irregObject
#' 
#' # get-methods
#' getxVal(irregObject)
#' getX(irregObject)
#' 
#' newIrregObject <- setxVal(irregObject, list(0:4, 1:3))
#' newIrregObject <- setX(irregObject, list(12:16, 13:15))
#' 
#' ### Multivariate
#' multiObject <- multiFunData(object, funData(xVal = 1:3, X = rbind(3:5, 6:8)))
#' multiObject
#' 
#' # get-methods
#' getxVal(multiObject)
#' getX(multiObject)
#' 
#' # set-methods (for special cases see univariate version)
#' multiObject <- setxVal(multiObject, list(5:1, 3:1))
#' multiObject <- setX(multiObject, list(rbind(5:1, 10:6), rbind(5:3, 8:6)))
setGeneric("getxVal", function(object) {standardGeneric("getxVal")})

#' Get xVal slot for funData objects
#'
#' @seealso \link{getxVal}
#'
#' @keywords internal
setMethod("getxVal", signature = "funData",
          function(object){object@xVal})

#' Get xVal slot for multiFunData objects
#'
#' @seealso \link{getxVal}
#'
#' @keywords internal
setMethod("getxVal", signature = "multiFunData",
          function(object){sapply(object, getxVal)})

#' Get xVal slot for irregular functional data objects
#'
#' @seealso \link{getxVal}
#'
#' @keywords internal
setMethod("getxVal", signature = "irregFunData",
          function(object){object@xVal})


#'@rdname getxVal
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



#' @rdname getxVal
#'
#' @export setxVal
setGeneric("setxVal", function(object, newxVal) {standardGeneric("setxVal")})

#' Set xVal slot for funData objects
#'
#' @seealso \link{setxVal}
#'
#' @keywords internal
setMethod("setxVal", signature = "funData",
          function(object, newxVal){
            if(is.numeric(newxVal))
              newxVal <- list(newxVal)
            object@xVal <- newxVal; validObject(object); return(object)
          })

#' Set xVal slot for multiFunData objects
#'
#' @seealso \link{setxVal}
#'
#' @keywords internal
setMethod("setxVal", signature = "multiFunData",
          function(object, newxVal){
            if(length(object)!=length(newxVal))
              stop("setxVal: multiFunData object and newxVal must have the same length")
            multiFunData(mapply(setxVal, object, newxVal))
          })


#' Set xVal slot for irregular functional objects
#'
#' @seealso \link{setxVal}
#'
#' @keywords internal
setMethod("setxVal", signature = "irregFunData",
          function(object, newxVal){
            if(length(object@xVal) != length(newxVal))
              stop("setxVal: newxVal must be a list of the same length as the original xVal.")
            
            if(any(sapply(object@xVal, function(l){length(l)}) != sapply(newxVal, function(l){length(l)})))
              stop("setxVal: newxVal must have the same structure as the original xVal.")
            
            object@xVal <- newxVal
            
            return(object)
          })


#' @rdname getxVal
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
#' xVal <- seq(0,2*pi,0.01)
#' refData <- funData(xVal, rbind(sin(xVal))) # one observation as reference
#' newData <- funData(xVal, outer(sample(c(-1,1), 11, replace = TRUE) * seq(0.75, 1.25, by = 0.05),
#'                                sin(xVal)))
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
#' ind <- replicate(11, sort(sample(1:length(xVal), sample(5:10,1)))) # sample observation points
#' xValIrreg <- lapply(ind, function(i){xVal[i]})
#' xValIrregAll <- unique(sort(unlist(xValIrreg)))
#'  # one observation as reference (fully observed)
#' refDataFull <- funData(xVal, rbind(sin(xVal)))
#'  # one observation as reference (irregularly observed)
#' refDataIrreg <- irregFunData(xVal = list(xValIrregAll), X = list(sin(xValIrregAll)))
#' newData <- irregFunData(xVal = xValIrreg, X = mapply(function(x, a, s){s * a * sin(x)},
#'      x = xValIrreg, a = seq(0.75, 1.25, by = 0.05), s = sample(c(-1,1), 11, replace = TRUE)))
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
#' refData <- multiFunData(funData(xVal, rbind(sin(xVal))), # one observation as reference
#'                         funData(xVal, rbind(cos(xVal)))) 
#' sig <- sample(c(-1,1), 11, replace = TRUE) 
#' newData <- multiFunData(funData(xVal, outer(sig * seq(0.75, 1.25, by = 0.05), sin(xVal))),
#'                         funData(xVal, outer(sig * seq(0.75, 1.25, by = 0.05), cos(xVal))))
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
            
            if(!isTRUE(all.equal(refObject@xVal, newObject@xVal)))
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
            
            if(!isTRUE(all.equal(getxVal(refObject), getxVal(newObject))))
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
            
            if(!all(unlist(newObject@xVal) %in% unlist(refObject@xVal)))
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
            
            if(!all(unlist(newObject@xVal) %in% unlist(refObject@xVal)))
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
#' i1 <- irregFunData(xVal = list(1:3,1:3,1:3), X = list(1:3,2:4,3:5))
#' all.equal(meanFunction(i1), extractObs(i1, obs = 2))
#' # don't run: functions are not defined on the same domain
#' \dontrun{meanFunction(irregFunData(xVal = list(1:3,1:5), X = list(1:3,1:5))) }
setGeneric("meanFunction", function(object, na.rm = FALSE) {standardGeneric("meanFunction")})


#' Mean for functional data
#'
#' @seealso \link{meanFunction}
#'
#' @keywords internal
setMethod("meanFunction", signature = c("funData", "ANY"),
          function(object, na.rm){
            funData(xVal = object@xVal, X = array(apply(object@X, 1+1:dimSupp(object), mean, na.rm = na.rm),
                                                  c(1,dim(object@X)[-1]))) # resize to array with one observation
          })
            
#' Mean for multivariate functional data
#'
#' @seealso \link{meanFunction}
#'
#' @keywords internal
setMethod("meanFunction", signature = c("multiFunData", "ANY"),
          function(object, na.rm){
            multiFunData(lapply(object, meanFunction, na.rm))
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
            
            if(!all(sapply(object@xVal[-1], function(x){isTRUE(all.equal(object@xVal[[1]], x))})))
              stop("Mean function defined only for irregular functional data objects on the same domain.")
            
            irregFunData(object@xVal[1], list(sapply(object@X, mean)))
          })