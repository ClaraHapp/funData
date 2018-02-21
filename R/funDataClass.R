#' @import methods
NULL

#' A class for (univariate) functional data
#' 
#' The \code{funData} class represents functional data on \eqn{d}{d}-dimensional
#' domains. The two slots represent the domain (x-values) and the values of the 
#' different observations (y-values).
#' 
#' Functional data can be seen as realizations of a random process \deqn{X: 
#' \mathcal{T} \to \mathrm{IR}}{X: \calT -> IR} on a \eqn{d}{d}-dimensional 
#' domain \eqn{\mathcal{T}}{\calT}. The data is usually sampled on a fine grid 
#' \eqn{T \subset \mathcal{T}}{T subset of \calT}, which is represented in the 
#' \code{argvals} slot of a \code{funData} object. All observations are assumed 
#' to be sampled over the same grid \eqn{T}{T}, but can contain missing values 
#' (see below). If \eqn{\mathcal{T}}{\calT} is one-dimensional, \code{argvals} 
#' can be supplied either as a numeric vector, containing the x-values or as a 
#' list, containing such a vector. If \eqn{\mathcal{T}}{\calT} is 
#' higher-dimensional, \code{argvals} must always be supplied as a list, 
#' containing numeric vectors of the x-values in dimensions 
#' \eqn{1,\ldots,d}{1,\ldots,d}.
#' 
#' The observed values are represented in the \code{X} slot of a \code{funData} 
#' object, which is an array of dimension  \eqn{N \times M}{N x M} (for 
#' one-dimensional domains, or \eqn{N \times M_1 \times \ldots \times M_d}{N x 
#' M_1 x \ldots x M_d} for higher-dimensional domains). Here \eqn{N}{N} equals 
#' the number of observations and \eqn{M}{M} denotes the number of sampling 
#' points (for higher dimensional domains \eqn{M_i}{M_i} denotes the number of
#' sampling points in dimension \eqn{i, i = 1,\ldots, d}{i, i = 1,\ldots, d}).
#' Missing values in the observations are allowed and must be marked by
#' \code{NA}. If missing values occur due to irregular observation points, the
#' data can be stored alternatively as an object of class
#' \code{\linkS4class{irregFunData}}.
#' 
#' Generic functions for the \code{funData} class include a print method, 
#' \link[=plot.funData]{plotting} and \link[=Arith.funData]{basic arithmetics}. 
#' Further methods for \code{funData}: \itemize{ \item \code{\link{dimSupp}}, 
#' \code{\link{nObs}}: Informations about the support dimensions and the number 
#' of observations, \item \code{\link{getArgvals}}, \code{\link{extractObs}}: 
#' Getting/Setting slot values (instead of accessing them directly via 
#' \code{funData@@argvals, funData@@X}) and extracting single observations or 
#' data on a subset of the domain, \item \code{\link{integrate}}, 
#' \code{\link{norm}}: Integrate all observations over their domain or 
#' calculating the \eqn{L^2}{L^2} norm.}
#' 
#' A \code{funData} object can be coerced to a \code{multiFunData} object using 
#' \code{as.multiFunData(funDataObject).}
#' 
#' @slot argvals The domain \eqn{\mathcal{T}}{\calT} of the data. See Details.
#' @slot X The functional data samples. See Details.
#'   
#' @aliases funData
#'   
#' @seealso \code{\linkS4class{irregFunData}}, \code{\linkS4class{multiFunData}}
#'   
#' @examples
#' ### Creating a one-dimensional funData object with 2 observations
#' # Basic
#' f1 <- new("funData", argvals = list(1:5), X = rbind(1:5,6:10))
#' # Using the constructor with first argument supplied as array
#' f2 <- funData(argvals = list(1:5), X = rbind(1:5, 6:10)) 
#' # Using the constructor with first argument supplied as numeric vector
#' f3 <- funData(argvals = 1:5, X = rbind(1:5, 6:10)) 
#' # Test if all the same
#' all.equal(f1,f2) 
#' all.equal(f1,f3)
#' # Display funData object in the console
#' f3 
#' 
#' # A more realistic object
#' argvals <- seq(0,2*pi,0.01)
#' object <- funData(argvals, outer(seq(0.75, 1.25, by = 0.05), sin(argvals)))
#' # Display / summary give basic information
#' object 
#' summary(object)
#' # Use the plot function to get an impression of the data
#' plot(object) 
#' 
#' 
#' ### Higher-dimensional funData objects with 2 observations
#' # Basic
#' g1 <- new("funData", argvals = list(1:5, 1:3),
#'                      X = array(1:30, dim = c(2,5,3))) 
#' # Using the constructor
#' g2 <- funData(argvals = list(1:5, 1:3),
#'               X = array(1:30, dim = c(2,5,3)))
#' # Test if the same
#' all.equal(g1,g2)
#' # Display funData object in the console
#' g2 
#' # Summarize information
#' summary(g2)
setClass("funData", representation = representation(argvals = "list", X = "array"))


# Validity checks for funData objects
setValidity("funData", function(object){
    if(!all(sapply(object@argvals, is.numeric, simplify = TRUE)))
      return("All argvals elements must be numeric")
  
    if(length(object@argvals) != length(dim(object@X)[-1]))
      return("argvals and X element have different support dimensions! X-Dimensions must be of the form N x M1 x ... x Md")
    
    if(!all(dim(object@X)[-1] == sapply(object@argvals, length, simplify = TRUE)))
      return("argvals and X have different number of sampling points! X-Dimensions must be of the form N x M1 x ... x Md")
  
  return(TRUE)
})

#' Constructor for functional data objects, first argument (argvals) passed as list or vector of numerics
#' 
#' @param argvals A list of numeric vectors or a single numeric vector, giving the 
#'   sampling points in the domains. See Details.
#' @param X An array of dimension  \eqn{N \times M}{N x M} (for one-dimensional
#'   domains, or \eqn{N \times M_1 \times \ldots \times M_d}{N x M_1 x \ldots x
#'   M_d} for higher-dimensional domains), giving the observed values for
#'   \eqn{N}{N} individuals. Missing values can be included via \code{NA}. See
#'   Details.
#'   
#' @seealso \code{\linkS4class{funData}}
#'
#' @name funData-constructor
#' 
#' @docType methods
#' 
#' @export funData
#' 
#' @keywords internal
setGeneric("funData", function(argvals, X){standardGeneric("funData")})


#' @describeIn funData Constructor for functional data objects with \code{argvals} given as list.
#' 
#' @param argvals A list of numeric vectors or a single numeric vector, giving the 
#'   sampling points in the domains. See Details.
#' @param X An array of dimension  \eqn{N \times M}{N x M} (for one-dimensional
#'   domains, or \eqn{N \times M_1 \times \ldots \times M_d}{N x M_1 x \ldots x
#'   M_d} for higher-dimensional domains), giving the observed values for
#'   \eqn{N}{N} individuals. Missing values can be included via \code{NA}. See
#'   Details.
#' 
#' @docType methods
setMethod("funData", signature = c(argvals = "list", X = "array"),
          function(argvals, X){new("funData", argvals = argvals, X = X)})


#' @describeIn funData Constructor for functional data objects with \code{argvals}
#'   given as vector of numerics (only valid for one-dimensional domains).
#'   
#' @docType methods
setMethod("funData", signature = c(argvals = "numeric", X = "array"), 
          function(argvals, X){new("funData", argvals = list(argvals), X = X)})



#' A class for multivariate functional data
#' 
#' The \code{multiFunData} class represents multivariate functional data on 
#' (potentially) different domains, i.e. a multivariate functional data object 
#' is a vector of (univariate) functional data objects, just as a vector in 
#' \eqn{\mathrm{IR}^n}{IR^n} is a vector of \eqn{n}{n} scalars. In this implementation, a 
#' \code{multiFunData} object is represented as a list of univariate 
#' \code{funData} objects, see Details.
#' 
#' A \code{multiFunData} object is represented as a list of univariate 
#' \code{funData} objects, each having a \code{argvals} and \code{X} slot, 
#' representing the x-values and the observed y-values (see the 
#' \code{\link{funData}} class). When constructing a \code{multiFunData} object,
#' the  elements can be supplied as a list of \code{funData} objects or can be
#' passed directly as arguments to the constructor function.
#' 
#' Most functions implemented for the \code{\link{funData}} class are also 
#' implemented for \code{multiFunData} objects. In most cases, they simply apply
#' the corresponding univariate method to each element of the multivariate 
#' object and return it as a vector (if the result of the univariate function is
#' scalar, such as \code{\link{dimSupp}}) or as a \code{multiFunData} object (if
#' the result of the univariate function is a \code{funData} object, such as 
#' \code{\link{extractObs}}).
#' 
#' The norm of a multivariate functional data \eqn{f = (f_1 , \ldots, f_p)}{f =
#' (f_1 , \ldots, f_p)} is defined as \deqn{||| f ||| := \left(\sum_{j=1}^p ||
#' f_j ||^2 \right) ^{1/2}.}{||| f ||| := ( \sum || f_j ||^2 )^{1/2}.}
#' 
#' A \code{funData} object can be coerced to a \code{multiFunData} object with
#' one element using \code{as.multiFunData(funDataObject).}
#' 
#' @aliases multiFunData
#' 
#' @seealso \code{\linkS4class{funData}}
#' 
#' @importFrom stats rnorm
#'   
#' @examples
#' ### Creating a multifunData object with 2 observations on the same domain
#' # Univariate elements
#' x <- 1:5
#' f1 <- funData(x, rbind(x, x+1))
#' f2 <- funData(x,rbind(x^2, sin(x)))
#' # Basic
#' m1 <- new("multiFunData", list(f1,f2))
#' # Using the constructor, passing the elements as list
#' m2 <- multiFunData(list(f1,f2))
#' # Using the constructor, passing the elements directly
#' m3 <- multiFunData(f1,f2)
#' # Test if all the same
#' all.equal(m1,m2)
#' all.equal(m1,m3)
#' # Display multiFunData object in the console
#' m3
#' # Summarize
#' summary(m3)
#' 
#' ### Creating a multifunData object with 2 observations on different domains (both 1D)
#' # A new element
#' y <- 1:3
#' g1 <- funData(y, rbind(3*y, y+4))
#' # Create the multiFunData object
#' m4 <- multiFunData(f1,g1)
#' # Display multiFunData object in the console
#' m4
#' 
#' ### Creating a multifunData object with 2 observations on different domains (1D and 2D)
#' # A new element
#' y <- 1:3; z <- 1:4
#' g2 <- funData(list(y,z), array(rnorm(24), dim = c(2,3,4)))
#' # Create the multiFunData object
#' m5 <- multiFunData(f1,g2)
#' # Display multiFunData object in the console
#' m5
#' 
#' ### A more realistic object
#' # element 1
#' x <- seq(0,2*pi, 0.01)
#' f1 <- funData(x, outer(seq(0.75, 1.25, length.out = 6), sin(x)))
#' # element 2
#' y <- seq(-1,1, 0.01); z <- seq(-0.5, 0.5, 0.01)
#' X2 <- array(NA, c(6, length(y), length(z)))
#' for(i in 1:6) X2[i,,] <- outer(y, z, function(x,y){sin(i*pi*y)*cos(i*pi*z)})
#' f2 <- funData(list(y,z), X2)
#' # MultiFunData Object
#' m6 <- multiFunData(f1,f2)
#' # Display multiFunData object in the console for basic information
#' m6
#' # Summarize
#' summary(m6)
#' # Use the plot function to get an impression of the data
#' \dontrun{plot(m6)} # m6 has 2D element, must specify one observation for plotting
#' plot(m6, obs = 1, main = c("1st element (obs 1)", "2nd element (obs 1)"))
#' plot(m6, obs = 6, main = c("1st element (obs 6)", "2nd element (obs 6)"))
setClass("multiFunData", representation = "list")


# Validity check for multiFunData objects
setValidity("multiFunData", function(object){
 if(!all(sapply(object, is, "funData", simplify = TRUE)))
    return("Elements of multiFunData must be of class funData!")
  
  if(diff(range(sapply(object,nObs)))!= 0)
    return("All elements must have the same number of observations!")
  
  return(TRUE)
})


#' Constructor for multivariate functional data objects
#' 
#' @seealso \code{\linkS4class{multiFunData}}
#' 
#' @name multiFunData-constructor
#' 
#' @docType methods
#' 
#' @export multiFunData
#' 
#' @keywords internal
setGeneric("multiFunData", function(...){standardGeneric("multiFunData")})


#' @describeIn multiFunData  Constructor for multivariate functional data
#'   objects.
#'   
#' @param ... A list of funData objects or several funData objects passed as one
#'   argument, each. See Details.
#'   
#' @docType methods
setMethod("multiFunData", signature = "ANY", 
          function(...){ 
            l <- list(...)
            if(!is.list(l[[1]]))
              new("multiFunData", l)
            else
              new("multiFunData", l[[1]])
          })



#' Coerce a funData object to class multiFunData
#' 
#' @seealso \code{\linkS4class{funData}}, \code{\link{as.multiFunData}}
#' 
#' @name funData-setAs
#' 
#' @keywords internal
setAs("funData", "multiFunData", 
      def = function(from){ new("multiFunData", list(from))})



#' Coerce a funData object to class multiFunData
#' 
#' Coerce a \code{funData} object to class \code{multiFunData} with one element.
#' 
#' @param object The \code{funData} object that is to be converted to a
#'   \code{multiFunData} object of length 1.
#'   
#' @seealso \code{\linkS4class{funData}}, \code{\linkS4class{multiFunData}}
#'   
#' @export as.multiFunData
#' 
#' @examples
#' # create funData object with 5 observations
#' x <- seq(0,1,0.01)
#' f1 <- funData(argvals = x, X = 1:5 %o% x)
#' f1
#' class(f1)
#' 
#' # coerce to multiFunData object (of length 1)
#' m1 <- as.multiFunData(f1)
#' m1
#' class(m1)
setGeneric("as.multiFunData", function(object){standardGeneric("as.multiFunData")})


#' @rdname as.multiFunData
setMethod("as.multiFunData", signature = "funData", 
          function(object){as(object, "multiFunData")})


#' A class for irregularly sampled functional data
#' 
#' The \code{irregFunData} class represents functional data that is sampled 
#' irregularly on one-dimensional domains. The two slots represent the 
#' observation points (x-values) and the observed function values (y-values).
#' 
#' Irregular functional data are realizations of a random process \deqn{X: 
#' \mathcal{T} \to \mathrm{IR},}{X: \calT -> IR,} where each realization 
#' \eqn{X_i} of \eqn{X} is given on an individual grid \eqn{T_i \subset 
#' \mathcal{T}}{T_i \subset \calT} of observation points. As for the 
#' \code{\linkS4class{funData}} class, each object of the \code{irregFunData}
#' class has two slots; the \code{argvals} slot represents the observation
#' points and the \code{X} slot represents the observed data. In contrast to the
#' regularly sampled data, both slots are defined as lists of vectors, where
#' each entry corresponds to one observed function: \itemize{\item
#' \code{argvals[[i]]} contains the vector of observation points \eqn{T_i} for
#' the i-th function, \item \code{X[[i]]} contains the corresponding observed
#' data \eqn{X_i(t_{ij}), t_{ij} \in T_i}.}
#' 
#' Generic functions for the \code{irregFunData} class include a print method, 
#' \link[=plot.irregFunData]{plotting} and \link[=Arith.funData]{basic 
#' arithmetics}. Further methods for \code{irregFunData}: \itemize{ \item 
#' \code{\link{dimSupp}}, \code{\link{nObs}}: Informations about the support 
#' dimensions and the number of observations, \item \code{\link{getArgvals}}, 
#' \code{\link{extractObs}}: Getting/setting slot values (instead of accessing 
#' them directly via \code{irregObject@@argvals, irregObject@@X}) and extracting
#' single observations or data on a subset of the domain, \item 
#' \code{\link{integrate}}, \code{\link{norm}}: Integrate all observations over 
#' their domain or calculating the \eqn{L^2}{L^2} norm.}
#' 
#' An \code{irregFunData} object can be coerced to a \code{funData} object using
#' \code{as.funData(irregObject)}. The regular functional data object is defined
#' on the union of all observation grids of the irregular object. The value of 
#' the new object is marked as missing (\code{NA}) for observation points that 
#' are in the union, but not in the original observation grid.
#' 
#' @section Warning: Currently, the class is implemented only for functional 
#'   data on one-dimensional domains \eqn{\mathcal{T} \subset \mathrm{IR}}{\calT
#'   \subset IR}.
#'   
#' @slot argvals A list of numerics, representing the observation grid \eqn{T_i}
#'   for each realization \eqn{X_i} of \eqn{X}.
#' @slot X A list of numerics, representing the values of each observation 
#'   \eqn{X_i} of \eqn{X} on the corresponding observation points \eqn{T_i}.
#'   
#' @aliases irregFunData
#'   
#' @seealso \code{\linkS4class{funData}}, \code{\linkS4class{multiFunData}}
#'   
#' @examples
#' # Construct an irregular functional data object
#' i1 <- irregFunData(argvals = list(1:5, 2:4), X = list(2:6, 3:5))
#' # Display in the console
#' i1
#' # Summarize
#' summary(i1)
#' 
#' # A more realistic object
#' argvals <- seq(0,2*pi, 0.01)
#' ind <- replicate(11, sort(sample(1:length(argvals), sample(5:10,1)))) # sample observation points
#' argvalsIrreg <- lapply(ind, function(i){argvals[i]})
#' i2 <- irregFunData(argvals = argvalsIrreg, X = mapply(function(x, a){a * sin(x)},
#'              x = argvalsIrreg, a = seq(0.75, 1.25, by = 0.05)))
#' # Display/summary gives basic information
#' i2
#' summary(i2)
#' # Use the plot function to get an impression of the data
#' plot(i2) 
setClass("irregFunData", representation = representation(argvals = "list", X = "list"))


# Validity checks for irregfunData objects
setValidity("irregFunData", function(object){
  if(any(sapply(object@argvals, function(l){!is.numeric(l)})))
    return("argvals must be supplied as list of numerics")
  
  if(any(sapply(object@X, function(l){!is.numeric(l)})))
    return("X must be supplied as list of numerics")
  
  if(length(object@argvals) != length(object@X))
    return("Different number of observations for argvals and X")
  
  if(any(mapply(function(x,y){dim(as.array(x)) != dim(as.array(y))}, object@argvals, object@X)))
    return("Different numbers of observation points in argvals and X")
  
  return(TRUE)
})


#' Constructor for irregular functional data objects
#' 
#' @seealso \code{\linkS4class{irregFunData}}
#' 
#' @name irregFunData-constructor
#' 
#' @docType methods
#' 
#' @export irregFunData
#' 
#' @keywords internal
setGeneric("irregFunData", function(argvals, X){standardGeneric("irregFunData")})


#' @describeIn irregFunData  Constructor for irregular functional data
#'   objects.
#'   
#' @param argvals A list of numerics, corresponding to the observation points for each realization \eqn{X_i} (see Details).
#' @param X A list of numerics, corresponding to the observed functions \eqn{X_i} (see Details).
#'   
#' @docType methods
setMethod("irregFunData", signature = c(argvals = "list", X = "list"),
          function(argvals, X){new("irregFunData", argvals = argvals, X = X)})


#' Coerce an irregFunData object to class funData
#' 
#' @seealso \code{\linkS4class{funData}}, \code{\link{as.funData}}
#'   
#' @name irregFunData-setAs
#'   
#' @keywords internal
#' 
#' @examples
#' # create irregFunData object with 2 observations
#' i1 <- irregFunData(argvals = list(1:5, 3:6), X = list(2:6, 4:7))
#' i1@@argvals # argvals and X are both lists with 2 entries
#' i1@@X
#' 
#' # coerce to funData object (with missing values)
#' f1 <- as.funData(i1)
#' # argvals is a list containing one vector
#' # (one-dimensional domain, union of all observation points)
#' f1@@argvals
#' # X is a matrix with 2 rows and missing values
#' f1@@X
setAs("irregFunData", "funData", 
      def = function(from){      
        #     if(dimSupp(from) > 1)
        #      stop("as.funData is implemented only for irregular functional data on one-dimensional domains.")
        
        argvals <- sort(unique(unlist(from@argvals)))
        
        X <- array(NA, dim = c(nObs(from), length(argvals)))
        
        for(i in 1:nObs(from))
          X[i, argvals %in% from@argvals[[i]]] <- from@X[[i]]
        
        res <- funData(argvals = argvals, X = X)
        names(res) <- names(from)
        
        return(res)})

#' Coerce an irregFunData object to class funData
#' 
#' This function coerces an object of class \code{irregFunData} to a
#' \code{funData} object with missing values, which is defined on the union of
#' all observation points.
#' 
#' @param object The \code{irregFunData} object that is to be converted to a
#'   \code{funData} object with missing values.
#'   
#' @seealso \code{\linkS4class{funData}}, \code{\linkS4class{irregFunData}}
#'   
#' @export as.funData
setGeneric("as.funData", function(object){standardGeneric("as.funData")})

#' @rdname as.funData
setMethod("as.funData", signature = "irregFunData", 
          function(object){as(object, "funData")})

#' Coerce a funData object to class irregFunData
#' 
#' @seealso \code{\linkS4class{funData}}, \code{\link{as.irregFunData}}
#'   
#' @name funData-setAs
#'   
#' @keywords internal
#' 
#' @examples
#' # create funData object with 5 observations
#' f <- simFunData(N = 5, M = 7, eValType = "linear",
#'                 eFunType = "Fourier", argvals = seq(0,1,0.01))$simData
#' 
#' # sparsify artificially
#' fSparse <- sparsify(f, minObs = 4, maxObs = 10)
#'
#' # coerce to irregFunData object
#' i <- as.irregFunData(fSparse)
#' i
setAs("funData", "irregFunData", 
      def = function(from){      
             if(dimSupp(from) > 1)
                stop("The funData object must be defined on a one-dimensional domain.")
        
        # simple apply does not work if data is in fact dense...
        res <- irregFunData(argvals = lapply(1:nObs(from), function(i, mat, vals){x <- mat[i,]; vals[!is.na(x)]}, mat = from@X, vals = from@argvals[[1]]),
                            X = lapply(1:nObs(from), function(i, mat){x <- mat[i,]; x[!is.na(x)]}, mat = from@X))
        
        names(res) <- names(from)
                            
        return(res)
})

#' Coerce a funData object to class irregFunData
#' 
#' This function coerces an object of class \code{funData} to a
#' \code{irregFunData} object.
#' 
#' @param object The \code{funData} object that is to be converted to a
#'   \code{irregFunData} object.
#'   
#' @seealso \code{\linkS4class{funData}}, \code{\linkS4class{irregFunData}}
#'   
#' @export as.irregFunData
setGeneric("as.irregFunData", function(object){standardGeneric("as.irregFunData")})

#' @rdname as.irregFunData
setMethod("as.irregFunData", signature = "funData", 
          function(object){as(object, "irregFunData")})