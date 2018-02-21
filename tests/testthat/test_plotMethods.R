context("Test plot methods")

pdf(file=NULL)

argvals <- seq(0,2*pi,0.01)
# 1D
object1D <- funData(argvals, outer(seq(0.75, 1.25, length.out = 11), sin(argvals)))

# 2D
X <- array(0, dim = c(2, length(argvals), length(argvals)))
X[1,,] <- outer(argvals, argvals, function(x,y){sin((x-pi)^2 + (y-pi)^2)})
X[2,,] <- outer(argvals, argvals, function(x,y){sin(2*x*pi) * cos(2*y*pi)})
object2D <- funData(list(argvals, argvals), X)

# multivariate
m <- multiFunData(extractObs(object1D, obs = 1:2), object2D)

# irreg
i <- as.irregFunData(object1D)

test_that("plots", {
  
  # funData 1D / 2D
  
  # check errors
  expect_error(plot(object1D, obs = "1"),
               "Parameter 'obs' must be a vector of numerics with values between 1 and 11.")
  expect_error(plot(object1D, obs = 0),
               "Parameter 'obs' must be a vector of numerics with values between 1 and 11.")
  expect_error(plot(object1D, obs = 50),
               "Parameter 'obs' must be a vector of numerics with values between 1 and 11.")
  expect_error(plot(object1D, plotNA = "Yes"), "Parameter 'plotNA' must be passed as a logical.")
  expect_error(plot(object1D, plotNA = c(TRUE, FALSE)), "Parameter 'plotNA' must be passed as a logical.")
  expect_error(plot(object1D, add = "Yes"), "Parameter 'add' must be passed as a logical.")
  expect_error(plot(object1D, add = c(TRUE, FALSE)), "Parameter 'add' must be passed as a logical.")

  expect_error(plot(funData(argvals = list(1:5,2:6,3:7), X = array(10*5*5*5, dim = c(10,5,5,5)))),
               "plot is implemented only for functional data with one- or two-dimensional domain")
  expect_error(plot(funData(argvals = list(1:5,2:6), X = array(10*5*5, dim = c(10,5,5))), obs = 1:2),
               "Specify one observation for plotting")
  expect_error(plot(funData(argvals = list(1:5,2:6), X = array(10*5*5, dim = c(10,5,5))), obs = 1, add = TRUE),
               "Option add = TRUE not implemented for images")
  
  # check functionality
  expect_null(plot(object1D, main = "One-dimensional functional data"))
  expect_null(plot(object1D, plotNA = TRUE))
   
  expect_null(plot(object2D, main = "Two-dimensional functional data (obs 1)", obs = 1))
  expect_null(plot(object2D, main = "Two-dimensional functional data (obs 1)", obs = 1, legend = FALSE))
  
  # multiFunData
  # check errors
  expect_error(plot(m, obs = "1"),
               "Parameter 'obs' must be a vector of numerics with values between 1 and 2.")
  expect_error(plot(m, obs = 0),
               "Parameter 'obs' must be a vector of numerics with values between 1 and 2.")
  expect_error(plot(m, obs = 50),
               "Parameter 'obs' must be a vector of numerics with values between 1 and 2.")
  expect_error(plot(m, dim = "1"),
               "Parameter 'dim' must be a vector of numerics with values between 1 and 2.")
  expect_error(plot(m, dim = 0),
               "Parameter 'dim' must be a vector of numerics with values between 1 and 2.")
  expect_error(plot(m, dim = 5),
               "Parameter 'dim' must be a vector of numerics with values between 1 and 2.")
  expect_error(plot(m, par.plot = 1:2),
               "Parameter 'par.plot' must be either NULL or passed as a list.")
  expect_error(plot(m, main = 1:3),
               "Parameter 'main' must be either NULL or have lengths 1 or 2.")
  expect_error(plot(m, ylim = list(1:2,3:4,4:5)),
               "The ylim argument must be either a vector (used for all elements) or a list with values for each element.", fixed = TRUE)
  
  # check functionality
  expect_null(plot(m, xlab = "x", ylab = "y", main = "title", obs = 1))
  expect_null(plot(m, xlab = c("x1", "x2"), ylab = c("y1", "y2"),
                   main = c("title1", "title2"), obs = 2, par.plot = list(cex = 5)))
  expect_null(plot(multiFunData(object1D, exp(object1D)), log = c("","y"), ylim = list(c(-2,2), c(0.1,4))))
  expect_null(plot(multiFunData(object1D + 2, exp(object1D)), log = "y", ylim = c(0.2,5)))
  
  # irregFunData
  # check errors
  expect_error(plot(i, obs = "1"),
               "Parameter 'obs' must be a vector of numerics with values between 1 and 11.")
  expect_error(plot(i, obs = 0),
               "Parameter 'obs' must be a vector of numerics with values between 1 and 11.")
  expect_error(plot(i, obs = 50),
               "Parameter 'obs' must be a vector of numerics with values between 1 and 11.")
  expect_error(plot(i, add = "Yes"), "Parameter 'add' must be passed as a logical.")
  expect_error(plot(i, add = c(TRUE, FALSE)), "Parameter 'add' must be passed as a logical.")
  
  expect_warning(plot(as.irregFunData(object1D), add = TRUE, log = "y"),
               "Parameter 'log' cannot be reset when 'add = TRUE'.", fixed = TRUE)
  
  # check functionality
  expect_null(plot(i))
  expect_null(plot(i, col = c("green", "red")))
})


test_that("ggplots", {
  
  # funData 1D / 2D
  # check errors
  expect_error(ggplot(object1D, obs = "1"),
               "Parameter 'obs' must be a vector of numerics with values between 1 and 11.")
  expect_error(ggplot(object1D, obs = 0),
               "Parameter 'obs' must be a vector of numerics with values between 1 and 11.")
  expect_error(ggplot(object1D, obs = 50),
               "Parameter 'obs' must be a vector of numerics with values between 1 and 11.")
  expect_error(ggplot(object1D, plotNA = "Yes"), "Parameter 'plotNA' must be passed as a logical.")
  expect_error(ggplot(object1D, plotNA = c(TRUE, FALSE)), "Parameter 'plotNA' must be passed as a logical.")
  expect_error(ggplot(object1D, add = "Yes"), "Parameter 'add' must be passed as a logical.")
  expect_error(ggplot(object1D, add = c(TRUE, FALSE)), "Parameter 'add' must be passed as a logical.")
  expect_error(ggplot(funData(argvals = list(1:5,2:6,3:7), X = array(10*5*5*5, dim = c(10,5,5,5)))),
               "plot is implemented only for functional data with one- or two-dimensional domain")
  expect_error(ggplot(funData(argvals = list(1:5,2:6), X = array(10*5*5, dim = c(10,5,5))), obs = 1:2),
               "Specify one observation for plotting")
  expect_error(ggplot(funData(argvals = list(1:5,2:6), X = array(10*5*5, dim = c(10,5,5))), obs = 1, add = TRUE),
               "Option add = TRUE not implemented for images")

  # check functionality
  expect_s3_class(ggplot(object1D), "ggplot")
  expect_s3_class(ggplot(object2D, obs = 1), "ggplot")
   
  # multiFunData
  # check errors
  expect_error(ggplot(m, dim = "1"),
               "Parameter 'dim' must be a vector of numerics with values between 1 and 2.")
  expect_error(ggplot(m, dim = 0),
               "Parameter 'dim' must be a vector of numerics with values between 1 and 2.")
  expect_error(ggplot(m, dim = 5),
               "Parameter 'dim' must be a vector of numerics with values between 1 and 2.")
  expect_error(ggplot(m, plotGrid = "Yes"),
               "Parameter 'plotGrid' must be passed as a logical.")
  expect_error(ggplot(m, plotGrid = c(TRUE, FALSE)),
               "Parameter 'plotGrid' must be passed as a logical.")
  
  # check functionality
  expect_equal(length(ggplot(multiFunData(object1D, 2*object1D), plotGrid = TRUE)), 2)
  expect_equal(length(ggplot(multiFunData(object1D, 2*object1D), plotGrid = FALSE)), 2)
  
  
  # irregFunData
  # check errors
  expect_error(ggplot(i, obs = "1"),
               "Parameter 'obs' must be a vector of numerics with values between 1 and 11.")
  expect_error(ggplot(i, obs = 0),
               "Parameter 'obs' must be a vector of numerics with values between 1 and 11.")
  expect_error(ggplot(i, obs = 50),
               "Parameter 'obs' must be a vector of numerics with values between 1 and 11.")
  expect_error(ggplot(i, add = "Yes"), "Parameter 'add' must be passed as a logical.")
  expect_error(ggplot(i, add = c(TRUE, FALSE)), "Parameter 'add' must be passed as a logical.")
  
  # check functionality
  expect_s3_class(ggplot(as.irregFunData(object1D)), "ggplot")
})

dev.off()
