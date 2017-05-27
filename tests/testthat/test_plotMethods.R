context("Test plot methods")

test_that("plots", {
  # check errors
  expect_error(plot(funData(argvals = list(1:5,2:6,3:7), X = array(10*5*5*5, dim = c(10,5,5,5)))),
               "plot is implemented only for functional data with one- or two-dimensional domain")
  expect_error(plot(funData(argvals = list(1:5,2:6), X = array(10*5*5, dim = c(10,5,5))), obs = 1:2),
               "plot: specify one observation for plotting")
  expect_error(plot(funData(argvals = list(1:5,2:6), X = array(10*5*5, dim = c(10,5,5))), obs = 1, add = TRUE),
               "Option add = TRUE not implemented for images")
  
  # check functionality
  argvals <- seq(0,2*pi,0.01)
  # 1D
  object1D <- funData(argvals, outer(seq(0.75, 1.25, length.out = 11), sin(argvals)))
  expect_null(plot(object1D, main = "One-dimensional functional data"))
  expect_null(plot(object1D, plotNA = TRUE))
  
  # 2D
  X <- array(0, dim = c(2, length(argvals), length(argvals)))
  X[1,,] <- outer(argvals, argvals, function(x,y){sin((x-pi)^2 + (y-pi)^2)})
  X[2,,] <- outer(argvals, argvals, function(x,y){sin(2*x*pi) * cos(2*y*pi)})
  object2D <- funData(list(argvals, argvals), X)
   
  expect_null(plot(object2D, main = "Two-dimensional functional data (obs 1)", obs = 1))
  expect_null(plot(object2D, main = "Two-dimensional functional data (obs 1)", obs = 1, legend = FALSE))
  
  # multivariate
  m <- multiFunData(extractObs(object1D, obs = 1:2), object2D)
  
  # check errors
  expect_error(plot(m, ylim = list(1:2,3:4,4:5)),
               "The ylim argument must be either a vector (used for all elements) or a list with values for each element.", fixed = TRUE)
  
  # check functionality
  expect_null(plot(m, xlab = "x", ylab = "y", main = "title", obs = 1))
  expect_null(plot(m, xlab = c("x1", "x2"), ylab = c("y1", "y2"),
                   main = c("title1", "title2"), obs = 2, par.plot = list(cex = 5)))
  expect_null(plot(multiFunData(object1D, exp(object1D)), log = c("","y"), ylim = list(c(-2,2), c(0.1,4))))
  expect_null(plot(multiFunData(object1D + 2, exp(object1D)), log = "y", ylim = c(0.2,5)))
  
  # irreg
  expect_warning(plot(as.irregFunData(object1D), add = TRUE, log = "y"),
               "Parameter 'log' cannot be reset when 'add = TRUE'.", fixed = TRUE)
  
  expect_null(plot(as.irregFunData(object1D)))
  expect_null(plot(as.irregFunData(object1D), col = c("green", "red")))
})


test_that("ggplots", {
  # check errors
  expect_error(ggplot(funData(argvals = list(1:5,2:6,3:7), X = array(10*5*5*5, dim = c(10,5,5,5)))),
               "plot is implemented only for functional data with one- or two-dimensional domain")
  expect_error(ggplot(funData(argvals = list(1:5,2:6), X = array(10*5*5, dim = c(10,5,5))), obs = 1:2),
               "plot: specify one observation for plotting")
  expect_error(ggplot(funData(argvals = list(1:5,2:6), X = array(10*5*5, dim = c(10,5,5))), obs = 1, add = TRUE),
               "Option add = TRUE not implemented for images")
  
  # check functionality
  argvals <- seq(0,2*pi,0.01)
  # 1D
  object1D <- funData(argvals, outer(seq(0.75, 1.25, length.out = 11), sin(argvals)))
  expect_s3_class(ggplot(object1D), "ggplot")
  
  # 2D
  X <- array(0, dim = c(2, length(argvals), length(argvals)))
  X[1,,] <- outer(argvals, argvals, function(x,y){sin((x-pi)^2 + (y-pi)^2)})
  X[2,,] <- outer(argvals, argvals, function(x,y){sin(2*x*pi) * cos(2*y*pi)})
  object2D <- funData(list(argvals, argvals), X)
  
  expect_s3_class(ggplot(object2D, obs = 1), "ggplot")
   
  # multivariate
  expect_equal(length(ggplot(multiFunData(object1D, 2*object1D), plotGrid = TRUE)), 2)
  expect_equal(length(ggplot(multiFunData(object1D, 2*object1D), plotGrid = FALSE)), 2)
  
  # irreg
  expect_s3_class(ggplot(as.irregFunData(object1D)), "ggplot")
})
