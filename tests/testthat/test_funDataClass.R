context("Test funData Class")

test_that("funData class constructor", {
  # validity
  expect_error(funData(xVal = "Test", X = matrix(2, nrow = 1)),
               "unable to find an inherited method") # xVal is neither list nor vector of numerics
  expect_error(funData(xVal = list(1,"Test"), X = array(1:2, dim = c(1,1,1))),
               "all xVal elements must be numeric") # xVal contains non-numeric
  expect_error(funData(xVal = list(1:5), X = list(cbind(1:5,6:10))), 
               "unable to find an inherited method") # X is not an array
  expect_error(funData(xVal = list(1:5), X = array(1:25, dim = c(1,5,5))), 
               "xVal and X element have different support dimensions! X-Dimensions must be of the form N x M1 x ... x Md") # X has wrong dimensional support
  expect_error(funData(xVal = list(1:5, 1:4), X = array(1:25, dim = c(1,5,5))),
               "xVal and X have different number of sampling points! X-Dimensions must be of the form N x M1 x ... x Md") # X has wrong dimension (sampling points)
  
  # acception of NA values
  expect_is(funData(xVal = c(1:5), X = matrix(c(1:19, NA), nrow = 4)), "funData")
  
  # special cases: one-dimensional domain
  expect_equal(funData(xVal = 1:5, X = matrix(1:20, nrow = 4)), funData(xVal = list(1:5), X = matrix(1:20, nrow = 4))) 
})

test_that("multiFunData class constructor", {
  f1 <- funData(xVal = 1:5, X = matrix(1:20, nrow = 4))
  
  # validity
  expect_error(multiFunData(list(5, 5)), 
               "elements of multiFunData must be of class funData!")
  expect_error(multiFunData(list(f1, funData(xVal = list(1:5, 6:10), X = array(1:125, c(5,5,5))))),
               "All elements must have the same number of observations!")
  
  # conversion
  expect_is(as(f1, "multiFunData"), "multiFunData")  
  
  # different construction methods
  expect_equal(multiFunData(f1,f1), multiFunData(list(f1,f1)))
})


