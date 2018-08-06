### Subsetting functional data objects

#' @rdname extractObs
#' @exportMethod [
setMethod("[", c("funData", "numeric", "missing", "missing"),
          function(x, i, j, ..., drop=TRUE)
          {
            cat(length(i), "\n")
            extractObs(x, obs = i)
          })

#' @rdname extractObs
#' @keywords internal
setMethod("[", c("multiFunData", "numeric", "missing", "missing"),
          function(x, i, j, ..., drop=TRUE)
          {
            extractObs(x, obs = i)
          })

#' @rdname extractObs
#' @keywords internal
setMethod("[", c("irregFunData", "numeric", "missing", "missing"),
          function(x, i, j, ..., drop=TRUE)
          {
            extractObs(x, obs = i)
          })




