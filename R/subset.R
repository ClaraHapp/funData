### Subsetting functional data objects

#' @rdname extractObs
#' @keywords internal
setMethod("[", c("funData", "integer", "missing", "missing"),
          function(x, i, j, ..., drop=TRUE)
          {
            extractObs(x, obs = i)
          })

#' @rdname extractObs
#' @keywords internal
setMethod("[", c("multiFunData", "integer", "missing", "missing"),
          function(x, i, j, ..., drop=TRUE)
          {
            extractObs(x, obs = i)
          })

#' @rdname extractObs
#' @keywords internal
setMethod("[", c("irregFunData", "integer", "missing", "missing"),
          function(x, i, j, ..., drop=TRUE)
          {
            extractObs(x, obs = i)
          })




