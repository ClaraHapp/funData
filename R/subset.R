### Subsetting functional data objects


#### extractObs ####

#' Extract observations of functional data
#'
#' This function extracts one or more observations and/or observations on
#' a part of the domain from a \code{funData}, \code{irregFunData} or
#' \code{multiFunData} object.
#'
#' In case of an \code{irregFunData} object, some functions may not have
#' observation points in the given part of the domain. In this case, the
#' functions are removed from the extracted dataset and a warning is
#' thrown.
#'
#' If only observations are to be extracted, the usual notation
#' \code{object[1:3]} is equivalent to \code{extractObs(object, obs =
#' 1:3)}. This works only if the domain remains unchanged.
#'
#' @section Warning: The function is currently implemented only for
#'   functional data with up to three-dimensional domains.
#'
#' @section Alias: The function \code{subset} is an alias for
#'   \code{extractObs}.
#'
#' @param object An object of class \code{funData}, \code{irregFunData} or
#'   \code{multiFunData}.
#' @param obs A numeric vector, giving the indices of the observations to
#'   extract (default: all obervations).
#' @param argvals The part of the domain to be extracted (default: the
#'   whole domain \code{object}@@\code{argvals}). Must be a list or a
#'   numeric vector (only for one-dimensional domains, see also the
#'   definition of \code{\linkS4class{funData}},
#'   \code{\linkS4class{multiFunData}}).
#'
#' @return An object of class \code{funData}, \code{irregFunData} or
#'   \code{multiFunData} containing the desired observations.
#'
#' @seealso \code{\linkS4class{funData}},
#'   \code{\linkS4class{irregFunData}}, \code{\linkS4class{multiFunData}}
#'
#' @export extractObs
#'
#' @examples
#' # Univariate - one-dimensional domain
#' object1 <- funData(argvals = 1:5, X = rbind(1:5, 6:10))
#' extractObs(object1, obs = 1)
#' object1[1]  # shorthand
#' extractObs(object1, argvals = 1:3)
#' extractObs(object1, argvals = list(1:3)) # the same as the statement before
#' # alias
#' subset(object1, argvals = 1:3)
#'
#' # Univariate - two-dimensional domains
#' object2 <- funData(argvals = list(1:5, 1:6), X = array(1:60, dim = c(2, 5, 6)))
#' extractObs(object2, obs = 1)
#' object2[1] # shorthand
#' extractObs(object2, argvals = list(1:3, c(2,4,6))) # argvals must be supplied as list
#'
#' # Univariate - irregular
#' irregObject <- irregFunData(argvals = list(1:5, 2:4), X = list(2:6, 3:5))
#' extractObs(irregObject, obs = 2)
#' irregObject[2] # shorthand
#' extractObs(irregObject, argvals = 1:3)
#' extractObs(irregObject, argvals = c(1,5)) # throws a warning, as second function has no observations
#'
#' # Multivariate
#' multiObject <- multiFunData(object1, object2)
#' extractObs(multiObject, obs = 2)
#' multiObject[2] # shorthand
#' extractObs(multiObject, argvals = list(1:3, list(1:3, c(2,4,6))))
setGeneric("extractObs", function(object, obs = 1:nObs(object), argvals = funData::argvals(object)) {
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

#### Alias: subset ####
#' @rdname extractObs
#' 
#' @param x An object of class \code{funData}, \code{irregFunData} or
#'   \code{multiFunData} (for \code{subset}).
#' @exportMethod subset
setMethod("subset", c("funData"),
          function(x, obs = 1:nObs(x), argvals = funData::argvals(x))
          {
            extractObs(x, obs = obs, argvals = argvals)
          })

#' @rdname extractObs
#' @exportMethod subset
setMethod("subset", c("multiFunData"),
          function(x, obs = 1:nObs(x), argvals = funData::argvals(x))
          {
            extractObs(x, obs = obs, argvals = argvals)
          })

#' @rdname extractObs
#' @exportMethod subset
setMethod("subset", c("irregFunData"),
          function(x, obs = 1:nObs(x), argvals = funData::argvals(x))
          {
            extractObs(x, obs = obs, argvals = argvals)
          })

#### Alternative via [ (only observations) ####

#' @describeIn extractObs
#'
#' @param i A numeric vector, giving the indices of the observations to
#'   extract when using \code{x[i]}. Defaults to all observations.
#' @param ... Used to pass further arguments to \code{extractObs}. Here
#'   only usable for \code{argvals}.
#' @param j,drop not used
#'
#' @exportMethod [
setMethod("[", c(x = "funData", i = "ANY", j = "missing", drop = "missing"),
          function(x, i, j, ..., drop)
          {
            if(missing(i)) # default value not found...
              i = 1:nObs(x)
            
            extractObs(x, obs = i, ...)
          })

#' @rdname extractObs
#' @exportMethod [
setMethod("[", c("multiFunData",  i = "ANY", j = "missing", drop = "missing"),
          function(x, i, j, ..., drop)
          {
            if(missing(i)) # default value not found...
              i = 1:nObs(x)
            
            extractObs(x, obs = i, ...)
          })

#' @rdname extractObs
#' @exportMethod [
setMethod("[", c("irregFunData",  i = "ANY", j = "missing", drop = "missing"),
          function(x, i = 1:nObs(x), j, ..., drop)
          {
            if(missing(i)) # default value not found...
              i = 1:nObs(x)
            
            extractObs(x, obs = i, ...)
          })