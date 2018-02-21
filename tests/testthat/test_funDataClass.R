context("Testing class definitions")

test_that("funData class constructor", {
  # validity
  expect_error(funData(argvals = "Test", X = matrix(2, nrow = 1)),
               "unable to find an inherited method") # argvals is neither list nor vector of numerics
  expect_error(funData(argvals = list(1,"Test"), X = array(1:2, dim = c(1,1,1))),
               "All argvals elements must be numeric") # argvals contains non-numeric
  expect_error(funData(argvals = list(1:5), X = list(cbind(1:5,6:10))), 
               "unable to find an inherited method") # X is not an array
  expect_error(funData(argvals = list(1:5), X = array(1:25, dim = c(1,5,5))), 
               "argvals and X element have different support dimensions! X-Dimensions must be of the form N x M1 x ... x Md") # X has wrong dimensional support
  expect_error(funData(argvals = list(1:5, 1:4), X = array(1:25, dim = c(1,5,5))),
               "argvals and X have different number of sampling points! X-Dimensions must be of the form N x M1 x ... x Md") # X has wrong dimension (sampling points)
  
  # acception of NA values
  expect_is(funData(argvals = c(1:5), X = matrix(c(1:19, NA), nrow = 4)), "funData")
  
  # special cases: one-dimensional domain
  expect_equal(funData(argvals = 1:5, X = matrix(1:20, nrow = 4)), funData(argvals = list(1:5), X = matrix(1:20, nrow = 4))) 
})


test_that("multiFunData class constructor", {
  f1 <- funData(argvals = 1:5, X = matrix(1:20, nrow = 4))
  
  # validity
  expect_error(multiFunData(list(5, 5)), 
               "Elements of multiFunData must be of class funData!")
  expect_error(multiFunData(list(f1, funData(argvals = list(1:5, 6:10), X = array(1:125, c(5,5,5))))),
               "All elements must have the same number of observations!")
  
  # conversion
  expect_is(as(f1, "multiFunData"), "multiFunData")  
  
  # different construction methods
  expect_equal(multiFunData(f1,f1), multiFunData(list(f1,f1)))
})


test_that("irregfunData class constructor", {
  # Validity:
  # argvals
  expect_error(irregFunData(argvals = "Test", X = list(5)),
               "unable to find an inherited method") # argvals is no list
  expect_error(irregFunData(argvals = list("Test"), X = list(5)),
               "argvals must be supplied as list of numerics")  # argvals is no list of numerics
  # X
  expect_error(irregFunData(argvals = list(5), X = "Test"),
               "unable to find an inherited method") # X is no list
  expect_error(irregFunData(argvals = list(5), X = list("Test")),
               "X must be supplied as list of numerics") # X is no list of numerics
  # relation between argvals and X
  expect_error(irregFunData(argvals = list(1:5), X = list(1:5, 2:4)),
               "Different number of observations for argvals and X")
  expect_error(irregFunData(argvals = list(1:5, 1:4), X = list(1:5, 2:4)),
               "Different numbers of observation points in argvals and X")
})

test_that("coerce methods", {
  x <- seq(0,1,0.01)
  f <- funData(argvals = x, X = 1:5 %o% x)
  i1 <- irregFunData(argvals = list(1:5, 3:6), X = list(2:6, 4:7))
  
  expect_error(as.irregFunData(tensorProduct(f,f)),
               "The funData object must be defined on a one-dimensional domain.")
 
  expect_equal(f, as.multiFunData(f)[[1]])
  expect_equal(as.funData(i1), {f1 <- funData(1:6, rbind(c(2:6, NA), c(NA,NA,4:7)))})
  expect_equal(unique(unlist(i1@argvals)), f1@argvals[[1]])
  expect_equal(i1@X, apply(f1@X, 1, na.omit), check.attributes = FALSE)
  expect_equal(i1, as.irregFunData(f1))
})

