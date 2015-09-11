context("Test funData methods")

test_that("dimSupp", {
  f1 <- funData(xVal = 1:5, X = matrix(1:20, nrow = 4))
  f2 <- funData(xVal = list(1:5, 6:10), X = array(100, c(4, 5, 5)))
  m1 <- multiFunData(f1, f2)
  
  # Check functionality:
  # univariate FD object (one-dim)
  expect_equal(dimSupp(f1), 1)
  # univariate FDobject (two-dim)
  expect_equal(dimSupp(f2), 2)
  # multivariate FD object
  expect_equal(dimSupp(m1), c(1, 2))
})

test_that("nObs", {
  f1 <- funData(xVal = 1:5, X = matrix(1:20, nrow = 4))
  f2 <- funData(xVal = list(1:5, 6:10), X = array(1:100, c(4, 5, 5)))
  m1 <- multiFunData(f1, f2)
  
  # Check functionality:
  # univariate FD object (one-dim)
  expect_equal(nObs(f1), 4)
  # univariate FD object (two-dim)
  expect_equal(nObs(f2), 4)
  # multivariate FD object
  expect_equal(nObs(m1), 4)
})

test_that("extractObs", {
  f1 <- funData(xVal = 1:5, X = matrix(1:20, nrow = 4))
  f2 <- funData(xVal = list(1:5, 1:6), X = array(1:120, c(4, 5, 6)))
  m1 <- multiFunData(list(f1, f2))
  
  # Check errors:
  # univariate FD object (one-dim)
  expect_error(extractObs(f1, obs = 5), 
               "Trying to extract observations that do not exist!") # observation does not exist
  expect_error(extractObs(f1, xVal = list(4:6)), 
               "Trying to extract x-values that do not exist!") # xVals do not exist
  expect_error(extractObs(f1, xVal = "a"), # wrong data type
               "Supply xVals for exstracted observations either as list or as numeric vector (only if support is one-dimensional)", fixed = TRUE) # fixed, as '(...)' is interpreted as regexp
  # univariate FD object (two-dim)
  expect_error(extractObs(f2, xVal = 1:5),
               "Supply xVals for exstracted observations either as list or as numeric vector (only if support is one-dimensional", fixed = TRUE) # fixed, as '(...)' is interpreted as regexp
  
  # Check functionality:
  # univariate FD object (one-dim)
  expect_equal(extractObs(f1, obs = 1:2), funData(xVal = 1:5, matrix(1:20, nrow = 4)[1:2, ]))
  expect_equal(extractObs(f1, xVal = 1:2), funData(xVal = 1:2, matrix(1:20, nrow = 4)[, 1:2]))
  expect_equal(extractObs(f1, xVal = 1:2), extractObs(f1, xVal = list(1:2)))
  # univariate FDobject (two-dim)
  expect_equal(extractObs(f2, obs = 2),  funData(xVal = list(1:5, 1:6), X = array(1:120, c(4, 5, 6))[2, , , drop = FALSE]))
  expect_equal(extractObs(f2, xVal = list(1:3, 4:6)), funData(xVal = list(1:3, 4:6), X = array(1:120, c(4, 5, 6))[, 1:3, 4:6]))
  # multivariate FD object
  expect_equal(extractObs(m1, obs = 2), multiFunData(extractObs(m1[[1]], obs = 2), extractObs(m1[[2]], obs = 2)))  
})

test_that("Arith", {
  x <- seq(0,1,length.out = 100)
  f1 <- funData(x, t(replicate(40, sin(x) + rnorm(100, sd = 0.1))))
  m1 <- multiFunData(f1, funData(x, t(replicate(40, cos(x) + rnorm(100, sd = 0.1)))))
  
  # Check functionality:
  # univariate & univariate
  expect_equal(f1+f1, funData(x,f1@X+f1@X))
  expect_equal(f1-f1, funData(x,f1@X-f1@X))
  expect_equal(f1*f1, funData(x,f1@X*f1@X))
  expect_equal(f1/f1, funData(x,f1@X/f1@X))  
  # univariate & scalar
  expect_equal(f1+f1, 2*f1)
  expect_equal(f1+f1, f1*2)
  expect_equal(f1-f1, 0*f1)
  expect_equal(f1-f1, f1*0)
  expect_equal(f1*f1, f1^2)
  expect_equal(f1/f1, 0*f1+1)
  expect_equal(f1/f1, 1+ f1*0)  
  # multivariate & multivariate
  expect_equal(m1+m1, multiFunData(mapply("+", m1, m1)))
  expect_equal(m1-m1, multiFunData(mapply("-", m1, m1)))
  expect_equal(m1*m1, multiFunData(mapply("*", m1, m1)))
  expect_equal(m1/m1, multiFunData(mapply("/", m1, m1)))  
  # multivariate & scalar
  expect_equal(m1+m1, 2*m1)
  expect_equal(m1+m1, m1*2)
  expect_equal(m1-m1, 0*m1)
  expect_equal(m1-m1, m1*0)
  expect_equal(m1*m1, m1^2)
  expect_equal(m1/m1, 0*m1+1)
  expect_equal(m1/m1, 1 + m1*0) 
})

test_that("norm", {
  f1 <- funData(xVal = 1:5, X = matrix(1:20, nrow = 4))
  m1 <- multiFunData(f1,f1)
  
  # Check functionality:
  # univariate FD object
  expect_equal(norm(f1), # all observations
               apply((f1^2)@X, 1, function(f, xVal, method){.intWeights(xVal, method) %*% f}, xVal = f1@xVal[[1]], method = "trapezoidal") )
  expect_equal(norm(f1)[1:3], norm(f1, obs = 1:3))   # only some observations
  expect_equal(norm(f1, squared = FALSE)[2], sqrt(norm(f1)[2])) # squared option
  # multivariate FD object
  expect_equal(norm(m1), rowSums(sapply(m1, norm, simplify = TRUE))) # all observations
  expect_equal(norm(m1)[1], norm(m1, obs = 1)) # only one observation
  expect_equal(norm(m1, squared = FALSE), sqrt(rowSums(sapply(m1, norm, squared = TRUE, simplify = TRUE)))) # squared option
})

test_that("integrate", {
  f1 <- funData(xVal = 1:5, X = matrix(1:20, nrow = 4))
  f2 <- funData(xVal = list(1:5, 1:6), X = array(1:120,c(4,5,6)))
  m1 <- multiFunData(f1,f2)
  
  # special case for data with only one observation
  f1.1 <- funData(xVal = 1:5, X = matrix(1:5, nrow = 1))
  f2.1 <- funData(xVal = list(1:5, 1:6), X = array(1:30,c(1,5,6)))
  m1.1 <- multiFunData(list(f1.1,f2.1))
  
  #Check errors:  
  expect_warning(integrate(extractObs(f1, xVal = 1:2)), # method = trapezoidal and not enough observation points
                 "Trapezoidal quadrature is not applicable for functions with < 3 observation points. 'method' changed to 'midpoint'.")
  
  # Check functionality:
  # univariate FD objects
  expect_equal(integrate(f1)[1], sum(.intWeights(f1@xVal[[1]], "trapezoidal")*f1@X[1,]))
  expect_equal(integrate(f2)[1], as.numeric(t(.intWeights(f2@xVal[[1]], "trapezoidal")) %*%  f2@X[1,,] %*% .intWeights(f2@xVal[[2]], "trapezoidal")))
  # multivariate FD objects
  expect_equal(integrate(m1), as.numeric(integrate(f1) + integrate(f2)))
  expect_equal(integrate(m1.1), as.numeric(integrate(f1.1) + integrate(f2.1)))
 })

test_that("set/get", {
  f1 <- funData(xVal = 1:5, X = matrix(1:20, nrow = 4))
  f2 <- funData(xVal = list(1:5, 1:6), X = array(1:120,c(4,5,6)))
  m <- multiFunData(f1,f2)
  
  #Check errors:
  # univariate FD object (one-dim)
  expect_error(setxVal(f1, 1:6), 
               "xVal and X have different number of sampling points! X-Dimensions must be of the form N x M1 x ... x Md") # wrong number of sampling points (xVal)
  expect_error(setX(f1, matrix(1:24, nrow = 4)), 
               "xVal and X have different number of sampling points! X-Dimensions must be of the form N x M1 x ... x Md") # wrong number of sampling points (X)
  expect_warning(setX(f1, matrix(1:25, nrow = 5)), 'setX: Number of observations has changed') # warning: more observations
  # univariate FD object (two-dim)
  expect_error(setxVal(f2, 1:5), 
               "xVal and X element have different support dimensions! X-Dimensions must be of the form N x M1 x ... x Md") # wrong dimension (xVal)
  expect_error(setX(f2, matrix(1:20, nrow = 4)), 
               "xVal and X element have different support dimensions! X-Dimensions must be of the form N x M1 x ... x Md") # wrong dimension (X)
  # multivariate FD object
  expect_error(setxVal(m, list(1+1:5, list(2+1:5, 3+1:6), 4+1:5)), 'setxVal: multiFunData object and newxVal must have the same length') # wrong length (xVal, multiFunData)
  expect_error(setX(m, list(getX(f1), getX(f2), matrix(1:12, nrow = 4))), 'setX: multiFunData object and newX must have the same length\n') # wrong length (X, multiFunData)
  expect_error(setX(m, list(matrix(1:25, nrow = 5), array(1:120, c(4,5,6)))), 'setX: newX object must have the same number of observations in all elements!') # different number of observations
  expect_warning(setX(m, list(matrix(1:25, nrow = 5), array(1:150, c(5,5,6)))), 'setX: Number of observations has changed') # warning: more observations
  # Check functionality:
  # univariate FD object (one-dim)
  expect_equal(getxVal(setxVal(f1, list(1+1:5))), list(1+1:5))
  expect_equal(getxVal(setxVal(f1, 1+1:5)), list(1+1:5)) # special case: one-dimensional domain
  expect_equal(getX(setX(f1, matrix(1+1:20, nrow = 4))), matrix(1+1:20, nrow = 4))
  # univariate FD object (two-dim)
  expect_equal(getxVal(setxVal(f2, list(1+1:5, 2+1:6))), list(1+1:5, 2+1:6))  
  # multivariate FD object
  expect_equal(getxVal(setxVal(m, list(list(2+1:5), list(1+1:5, 3+1:6)))), list(list(2+1:5), list(1+1:5, 3+1:6)))
  expect_equal(getX(setX(m, list(matrix(1+1:20, nrow = 4), array(2+1:120, c(4,5,6))))), list(matrix(1+1:20, nrow = 4), array(2+1:120, c(4,5,6))))
  expect_equal(getxVal(setxVal(m, list(1+1:5, list(2+1:5, 3+1:6)))), list(list(1+1:5), list(2+1:5, 3+1:6))) # special case: one-dimensional domains
})

test_that("flipFun", {
  f1 <- funData(xVal = 1:5, X = matrix(1:20, nrow = 4))
  f2 <- funData(xVal = list(1:5, 1:6), X = array(1:120,c(4,5,6)))
  m1 <- multiFunData(list(f1,f2))
  
  # Check errors:
  # univariate FD object (one-dim)
  expect_error(flipFuns(f1,funData(xVal = list(1:5), X = array(1:30,c(6,5)))), 
               'flipFuns: Functions must have the same number of observations or use a single function as reference.') # not the same number of observations
  expect_error(flipFuns(f1,f2), 
               'flipFuns: Functions must have the dimension.') # not the same dimension
  expect_error(flipFuns(f1,funData(xVal = list(2:6), X = array(1:20,c(4,5)))), 
               'flipFuns: Functions must be defined on the same domain.') # not the same domain
  
  # Check functionality:
  # univariate FD object (one-dim)
  expect_equal(flipFuns(f1, -1*f1), f1)
  # univariate FD object (two-dim)
  expect_equal(flipFuns(f2, -1*f2), f2)
  # multivariate FD object
  expect_equal(flipFuns(m1, -1*m1), m1) 
})