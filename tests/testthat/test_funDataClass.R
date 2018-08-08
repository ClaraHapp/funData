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
 
  # coercion between methods
  expect_equal(f, as.multiFunData(f)[[1]])
  expect_equal(as.funData(i1), {f1 <- funData(1:6, rbind(c(2:6, NA), c(NA,NA,4:7)))})
  expect_equal(unique(unlist(i1@argvals)), f1@argvals[[1]])
  expect_equal(i1@X, apply(f1@X, 1, na.omit), check.attributes = FALSE)
  expect_equal(i1, as.irregFunData(f1))
  
  # coercion to data.frame
  expect_equal(head(as.data.frame(f), nObsPoints(f)), data.frame(obs = "1", argvals1 = x, X = f@X[1,]), check.attributes = FALSE)
  expect_equal(tail(as.data.frame(f), nObsPoints(f)), data.frame(obs = "5", argvals1 = x, X = f@X[5,]), check.attributes = FALSE)
  expect_equal(as.data.frame(as.multiFunData(f)), list(as.data.frame(f)))
  expect_equal(as.data.frame(i1), data.frame(obs = rep(c("1","2"), times = c(5,4)), argvals = unlist(argvals(i1)), X = unlist(X(i1))))
  
  # coercion to fd from fda package
  if(!(requireNamespace("fda", quietly = TRUE)))
  {
    expect_warning(funData2fd(f), "Please install the fda package to use the funData2fd function for funData objects.")
    expect_warning(fd2funData(NULL), "Please install the fda package to use the fd2funData function for funData objects.")
  } 
  else
  {
    library("fda")
    
    # from Data2fd help
    daybasis <- create.fourier.basis(c(0, 365), nbasis=65) 
    tempfd <- Data2fd(argvals = day.5, y = CanadianWeather$dailyAv[,,"Temperature.C"], daybasis)
    
    # check errors
    expect_error(funData2fd("fun", daybasis), "Argument is not of class 'funData'.")
    expect_error(funData2fd(funData(argvals = list(1:5, 1:4), X = 3:1 %o% 1:5 %o% 1:4)), "funData2fd is only defined for functions on one-dimensional domains.")
    expect_error(fd2funData(tempfd, letters[1:5]), "Parameter 'argvals' must be either a vector of argument values or a list containing such a vector.")
    
    # check functionality
    tempFun <- fd2funData(tempfd, argvals = day.5)
    tempFun2 <- fd2funData(tempfd, argvals = list(day.5))
    expect_equal(nObs(tempFun), 35)
    expect_equal(nObsPoints(tempFun), 365)
    expect_equal(mean(norm(tempFun)), 60906.17, tol = 1e-5)
    expect_equal(norm(tempFun)[1], 27068, tol = 1e-5)
    expect_equal(tempFun, tempFun2)
    
    reTempfd <- funData2fd(tempFun, daybasis)
    reTempfd$fdnames$time <- tempfd$fdnames$time # time names are not preserved
    expect_equal(tempfd, reTempfd)
    
  }
})

