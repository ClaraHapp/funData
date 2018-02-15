context("Test funData methods")

test_that("print",{
  f1 <- funData(argvals = 1:5, X = matrix(1:20, nrow = 4))
  expect_known_output(print(f1), file = "outputs/print_funData.out")
  expect_known_output(print(multiFunData(f1)), file = "outputs/print_multiFunData.out")
  expect_known_output(print(as.irregFunData(f1)), file = "outputs/print_irregFunData.out")
})

test_that("summary",{
  f1 <- funData(argvals = 1:5, X = matrix(1:20, nrow = 4))
  
  # check errors
  expect_error(funData:::print.summary.funData(summary(multiFunData(f1))), 
               "Argument is not of class 'summary.funData'.")
  expect_error(funData:::print.summary.multiFunData(summary(f1)), 
               "Argument is not of class 'summary.multiFunData'.")
  expect_error(funData:::print.summary.irregFunData(summary(f1)), 
               "Argument is not of class 'summary.irregFunData'.")
  
  # check functionality
  expect_known_output(print(summary(f1)), file = "outputs/summary_funData.out")
  expect_known_output(print(summary(multiFunData(f1))), file = "outputs/summary_multiFunData.out")
  expect_known_output(print(summary(as.irregFunData(f1))), file = "outputs/summary_irregFunData.out")
})

test_that("dimSupp", {
  f1 <- funData(argvals = 1:5, X = matrix(1:20, nrow = 4))
  f2 <- funData(argvals = list(1:5, 6:10), X = array(100, c(4, 5, 5)))
  m1 <- multiFunData(f1, f2)
  i1 <- irregFunData(argvals = list(1:5, 2:4), X = list(1:5, 2:4))
  
  # Check functionality:
  # univariate FD object (one-dim)
  expect_equal(dimSupp(f1), 1)
  # univariate FDobject (two-dim)
  expect_equal(dimSupp(f2), 2)
  # multivariate FD object
  expect_equal(dimSupp(m1), c(1, 2))
  # irreg FD object
  expect_equal(dimSupp(i1), 1)
})

test_that("nObs", {
  f1 <- funData(argvals = 1:5, X = matrix(1:20, nrow = 4))
  f2 <- funData(argvals = list(1:5, 6:10), X = array(1:100, c(4, 5, 5)))
  m1 <- multiFunData(f1, f2)
  i1 <- irregFunData(argvals = list(1:5, 2:4), X = list(1:5, 2:4))
  
  # Check functionality:
  # univariate FD object (one-dim)
  expect_equal(nObs(f1), 4)
  # univariate FD object (two-dim)
  expect_equal(nObs(f2), 4)
  # multivariate FD object
  expect_equal(nObs(m1), 4)
  # irreg FD object
  expect_equal(nObs(i1),2)
  
})


test_that("nObsPoints", {
  f1 <- funData(argvals = 1:5, X = matrix(1:20, nrow = 4))
  f2 <- funData(argvals = list(1:5, 6:10), X = array(1:100, c(4, 5, 5)))
  m1 <- multiFunData(f1, f2)
  i1 <- irregFunData(argvals = list(1:5, 2:4), X = list(1:5, 2:4))
  
  # Check functionality:
  # univariate FD object (one-dim)
  expect_equal(nObsPoints(f1), 5)
  # univariate FD object (two-dim)
  expect_equal(nObsPoints(f2), c(5,5))
  # multivariate FD object
  expect_equal(nObsPoints(m1), list(5,c(5,5)))
  # irreg FD object
  expect_equal(nObsPoints(i1), c(5,3))  
})

test_that("extractObs", {
  f1 <- funData(argvals = 1:5, X = matrix(1:20, nrow = 4))
  f2 <- funData(argvals = list(1:5, 1:6), X = array(1:120, c(4, 5, 6)))
  f3 <- funData(argvals = list(1:5, 1:6, 1:4), X = array(1:480, c(4, 5, 6, 4)))
  m1 <- multiFunData(list(f1, f2))
  i1 <- irregFunData(argvals = list(1:5, 1:3), X = list(2:6, 2:4))
  
  # Check errors:
  # univariate FD object (one-dim)
  expect_error(extractObs(f1, obs = 5), 
               "Trying to extract observations that do not exist!") # observation does not exist
  expect_error(extractObs(f1, argvals = list(4:6)), 
               "Trying to extract x-values that do not exist!") # argvals do not exist
  expect_error(extractObs(f1, argvals = "a"), # wrong data type
               "Supply argvals for extracted observations either as list or as numeric vector (only if support is one-dimensional)", fixed = TRUE) # fixed, as '(...)' is interpreted as regexp
  # univariate FD object (two-dim)
  expect_error(extractObs(f2, argvals = 1:5),
               "Supply argvals for extracted observations either as list or as numeric vector (only if support is one-dimensional", fixed = TRUE) # fixed, as '(...)' is interpreted as regexp
  # irreg FD object
  expect_error(extractObs(i1, obs = list(1:3)), 
               "Supply observations as numeric vector")
  expect_error(extractObs(i1, obs = 3),
               "Trying to extract observations that do not exist!")
  expect_error(extractObs(extractObs(i1, argvals = "1")),
               "Supply argvals for extracted observations either as list or as numeric vector")
  expect_error(extractObs(i1, argvals = 6),
               "Trying to extract x-values that do not exist!")
  expect_warning(extractObs(i1, argvals = 4:5),
                 "Some functions were not observed on the given argvals and therefore removed.")
  
  # Check functionality:
  # univariate FD object (one-dim)
  expect_equal(extractObs(f1, obs = 1:2), funData(argvals = 1:5, matrix(1:20, nrow = 4)[1:2, ]))
  expect_equal(extractObs(f1, argvals = 1:2), funData(argvals = 1:2, matrix(1:20, nrow = 4)[, 1:2]))
  expect_equal(extractObs(f1, argvals = 1:2), extractObs(f1, argvals = list(1:2)))
  # univariate FDobject (two-dim)
  expect_equal(extractObs(f2, obs = 2),  funData(argvals = list(1:5, 1:6), X = array(1:120, c(4, 5, 6))[2, , , drop = FALSE]))
  expect_equal(extractObs(f2, argvals = list(1:3, 4:6)), funData(argvals = list(1:3, 4:6), X = array(1:120, c(4, 5, 6))[, 1:3, 4:6]))
  # univariate FDobject (three-dim)
  expect_equal(extractObs(f3, obs = 4),  funData(argvals = f3@argvals, X = f3@X[4, , , , drop = FALSE]))
  expect_equal(extractObs(f3, argvals = list(1:3, 4:6, 2:4)), funData(argvals = list(1:3, 4:6, 2:4), X = f3@X[, 1:3, 4:6, 2:4]))
  # multivariate FD object
  expect_equal(extractObs(m1, obs = 2), multiFunData(extractObs(m1[[1]], obs = 2), extractObs(m1[[2]], obs = 2)))  
  # irreg FD object
  expect_equal(extractObs(i1, argvals = list(1:2)), extractObs(i1, argvals = 1:2))
  expect_equal(extractObs(i1, obs = 1), irregFunData(argvals = list(1:5), X = list(2:6)))
  expect_equal(extractObs(i1, argvals =2:3), irregFunData(argvals = list(2:3, 2:3), X = list(3:4, 3:4)))
})

test_that("Arith", {
  x <- seq(0,1,length.out = 100)
  ind <- replicate(10, sort(sample(1:100, sample(10:20, 1))))
  f1 <- funData(x, t(replicate(40, sin(x) + rnorm(100, sd = 0.1))))
  f2 <- funData(argvals = list(1:5, 1:6), X = array(1:120, c(4, 5, 6)))
  m1 <- multiFunData(f1, funData(x, t(replicate(40, cos(x) + rnorm(100, sd = 0.1)))))
  i1 <- irregFunData(argvals = lapply(ind, function(i, x){x[i]}, x = f1@argvals[[1]]),
                     X = lapply(1:10, function(i, y){y[i,ind[[i]]]}, y = f1@X))
  x1 <- unique(unlist(i1@argvals))
  
  # Check errors:
  # univariateFD, univariate FD
  expect_error(f1 + extractObs(f1,1:2), 
               "Arithmetics: nObs of funData objects is neither equal nor one.")
  
  
  # irreg & irreg
  expect_error(extractObs(i1, obs = 1) + i1,
               "Arithmetics: Multiple functions must be defined on subdomain of single function.")
  expect_error(i1 + extractObs(i1, obs = 1),
               "Arithmetics: Multiple functions must be defined on subdomain of single function.")
  expect_error(i1 + extractObs(i1, obs = 1:5),
               "Arithmethics: IrregFunData objects must have either the same number of observations or just one.")
  expect_error(i1 +  irregFunData(argvals = lapply(i1@argvals, function(l){l+1}), X = i1@X),
               "Arithmetics for two irregular functional data objects are defined only for functions on the same domain.")
   
  # irreg & reg
  expect_error(i1+setArgvals(extractObs(f1, obs = 1:10), x+2),
               "irregFunData object must be defined on a subdomain of the funData object!")
  expect_error(i1+f1,
               "funData object must have either one observation or the same number of observations as the irregFunData object")

  
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
  # univariate with e1/e2 having only one observation
  expect_equal(extractObs(f1 + extractObs(f1,1),1), extractObs(2*f1,1), check.attributes = FALSE)
  expect_equal(extractObs(f2 + extractObs(f2,1),1), extractObs(2*f2,1), check.attributes = FALSE)
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
  # irreg & irreg
  expect_equal(i1+i1, irregFunData(i1@argvals,mapply('+', i1@X, i1@X)))
  expect_equal(i1-i1, irregFunData(i1@argvals,mapply('-', i1@X, i1@X)))
  expect_equal(i1*i1, irregFunData(i1@argvals,mapply('*', i1@X, i1@X)))
  expect_equal(i1/i1, irregFunData(i1@argvals,mapply('/', i1@X, i1@X)))
  expect_equal(i1 + irregFunData(argvals = list(x1), X = list(rep(0, length(x1)))), i1)
  expect_equal(irregFunData(argvals = list(x1), X = list(rep(1, length(x1)))) + i1, 1+ i1)
  # irreg & reg
  expect_equal(i1 + extractObs(f1, obs = 1:10), extractObs(f1, obs = 1:10) + i1) # same number of observations
  expect_equal(i1 + extractObs(f1, obs = 1), extractObs(f1, obs = 1) + i1) # funData object has only one observation
  # irreg & scalar
  expect_equal(i1+i1, 2*i1)
  expect_equal(i1+i1, i1*2)
  expect_equal(i1-i1, 0*i1)
  expect_equal(i1-i1, i1*0)
  expect_equal(i1*i1, i1^2)
  expect_equal(i1/i1, 0*i1+1)
  expect_equal(i1/i1, 1 + i1*0)   
})


test_that("Math", {
  set.seed(1)
  
  argvals <- seq(0,1, 0.01)
  
  # funData
  f <- simFunData(argvals = argvals, N = 10, M = 5, eFunType = "Fourier", eValType = "linear")$simData

  expect_equal(exp(f), funData(argvals, exp(f@X)))
  expect_equal(sin(f)^2 + cos(f)^2, 0*f+1) # combination of Arith and math
  
  # irregFunData
  i <- as.irregFunData(sparsify(f, minObs = 5, maxObs = 10))
  
  expect_equal(exp(i), irregFunData(i@argvals, lapply(i@X,exp)))
  expect_equal(sin(i)^2 + cos(i)^2, 0*i+1) # combination of Arith and math
  
  # multiFunData
  m <- multiFunData(f, -1*f)
  
  expect_equal(exp(m), multiFunData(exp(f), exp(-1*f)))
  expect_equal(sin(m)^2 + cos(m)^2, 0*m+1) # combination of Arith and math
})

test_that("norm", {
  f1 <- funData(argvals = 1:5, X = matrix(1:20, nrow = 4))
  m1 <- multiFunData(f1,f1)
  x1 <- seq(-1,1, by = 0.01)
  x2 <- seq(-0.5, 0.5, by = 0.01)
  i1 <- irregFunData(list(x1,x2), list(x1^2, x2^2))
  
  # Check functionality:
  # univariate FD object
  expect_equal(norm(f1), # all observations
               apply((f1^2)@X, 1, function(f, argvals, method){.intWeights(argvals, method) %*% f}, argvals = f1@argvals[[1]], method = "trapezoidal") )
  expect_equal(norm(f1)[1:3], norm(f1, obs = 1:3))   # only some observations
  expect_equal(norm(f1, squared = FALSE)[2], sqrt(norm(f1)[2])) # squared option
  expect_equal(norm(f1, weight = 2), 2*norm(f1)) # weight (makes little sense for univariate funData objects...)
  # multivariate FD object
  expect_equal(norm(m1), rowSums(sapply(m1, norm, simplify = TRUE))) # all observations
  expect_equal(norm(m1)[1], norm(m1, obs = 1)) # only one observation
  expect_equal(norm(m1, squared = FALSE), sqrt(rowSums(sapply(m1, norm, squared = TRUE, simplify = TRUE)))) # squared option
  expect_equal(norm(m1, weight = c(2,1)), norm(multiFunData(sqrt(2)*f1,f1))) # with weight
  # irreg FD object  
  expect_equal(norm(i1), c(2/5, 1/80), tolerance = 5e-4) # result calculated explicitly
  expect_equal(norm(i1, fullDom = TRUE), c(2/5, 1/80 + 2*13/96), tolerance = 1e-1) # result calculated explicitly
  expect_equal(norm(i1, weight = 2), 2*norm(i1)) # weight (makes little sense for univariate funData objects...)
})


test_that("scalarProduct", {
  set.seed(1)
  f <- simFunData(N = 5, M = 7, eValType = "linear",
                  eFunType = "Fourier", argvals = seq(0,1,0.01))$simData
  g <- simFunData(N = 5, M = 4, eValType = "linear",
                  eFunType = "Poly", argvals = seq(0,1,0.01))$simData
  m <- multiFunData(f,g)
  i <- as.irregFunData(sparsify(f, minObs = 5, maxObs = 10))
  
  # Check errors
  expect_error(scalarProduct(m, as.multiFunData(f)),
               "multiFunData objects must have the same number of elements.")
  
  expect_error(scalarProduct(m, m, weight = c(-1,1)),
               "Weights must be non-negative.")
  
  expect_error(scalarProduct(m, m, weight = c(0,0)),
               "At least one weighting factor must be different from 0.")
  
  # Check functionality:
  # univariate FD objects
  s <- scalarProduct(f,g)
  expect_equal(length(s), nObs(f))
  expect_equal(s[1], 0.68327608)
  expect_equal(scalarProduct(f,f), norm(f, squared = TRUE))
  # multivariate FD object
  expect_equal(scalarProduct(m,m), norm(m, squared = TRUE))
  expect_equal(scalarProduct(m,m, weight = c(1,2)), norm(m, squared = TRUE, weight = c(1,2))) # with weights
  # irreg FD object  
  expect_equal(scalarProduct(i,i), norm(i, squared = TRUE))
  })

test_that("integrate", {
  f1 <- funData(argvals = 1:5, X = matrix(1:20, nrow = 4))
  f2 <- funData(argvals = list(1:5, 1:6), X = array(1:120,c(4,5,6)))
  m1 <- multiFunData(f1,f2)
  x1 <- seq(-1,1, by = 0.01)
  x2 <- seq(-0.5, 0.5, by = 0.01)
  i1 <- irregFunData(list(x1,x2), list(x1^2, x2^2))
  
  # special case for data with only one observation
  f1.1 <- funData(argvals = 1:5, X = matrix(1:5, nrow = 1))
  f2.1 <- funData(argvals = list(1:5, 1:6), X = array(1:30,c(1,5,6)))
  m1.1 <- multiFunData(list(f1.1,f2.1))
  
  #Check errors:  
  expect_warning(integrate(extractObs(f1, argvals = 1:2)), # method = trapezoidal and not enough observation points
                 "Trapezoidal quadrature is not applicable for functions with < 3 observation points. 'method' changed to 'midpoint'.")
  
  # Check functionality:
  # univariate FD objects
  expect_equal(integrate(f1)[1], sum(.intWeights(f1@argvals[[1]], "trapezoidal")*f1@X[1,]))
  expect_equal(integrate(f2)[1], as.numeric(t(.intWeights(f2@argvals[[1]], "trapezoidal")) %*%  f2@X[1,,] %*% .intWeights(f2@argvals[[2]], "trapezoidal")))
  # multivariate FD objects
  expect_equal(integrate(m1), as.numeric(integrate(f1) + integrate(f2)))
  expect_equal(integrate(m1.1), as.numeric(integrate(f1.1) + integrate(f2.1)))
  # irreg FD object 
  expect_equal(integrate(i1), c(2/3, 1/12), tolerance = 1e-4)
  expect_equal(integrate(i1, fullDom = TRUE), c(2/3, 7/12), tolerance = 5e-3)
})

test_that("integrate3D",{
  x <- seq(0,1, 0.02)
  y <- seq(-0.5,0.5, 0.02)
  z <- seq(1,2,0.02)
  
  nX <- length(x)
  nY <- length(y)
  nZ <- length(z)
  
  A <- array(NA, c(nX, nY, nZ))
  
  for(ix in 1:nX)
    for(iy in 1:nY)
      for(iz in 1:nZ)
        A[ix,iy,iz] <-  x[ix]*cos(pi*y[iy])*z[iz]^2
  
  all.equal(integrate3D(A, argvals = list(x,y,z)), 7/(3*pi), tolerance = 1e-3)
})

test_that("set/get", {
  f1 <- funData(argvals = 1:5, X = matrix(1:20, nrow = 4))
  f2 <- funData(argvals = list(1:5, 1:6), X = array(1:120,c(4,5,6)))
  m <- multiFunData(f1,f2)
  i1 <- irregFunData(argvals = list(1:5, 2:4), X = list(2:6, 3:5))
  
  #Check errors:
  # univariate FD object (one-dim)
  expect_error(setArgvals(f1, 1:6), 
               "argvals and X have different number of sampling points! X-Dimensions must be of the form N x M1 x ... x Md") # wrong number of sampling points (argvals)
  expect_error(setX(f1, matrix(1:24, nrow = 4)), 
               "argvals and X have different number of sampling points! X-Dimensions must be of the form N x M1 x ... x Md") # wrong number of sampling points (X)
  expect_warning(setX(f1, matrix(1:25, nrow = 5)), 'setX: Number of observations has changed') # warning: more observations
  # univariate FD object (two-dim)
  expect_error(setArgvals(f2, 1:5), 
               "argvals and X element have different support dimensions! X-Dimensions must be of the form N x M1 x ... x Md") # wrong dimension (argvals)
  expect_error(setX(f2, matrix(1:20, nrow = 4)), 
               "argvals and X element have different support dimensions! X-Dimensions must be of the form N x M1 x ... x Md") # wrong dimension (X)
  # multivariate FD object
  expect_error(setArgvals(m, list(1+1:5, list(2+1:5, 3+1:6), 4+1:5)), 'setArgvals: multiFunData object and newArgvals must have the same length') # wrong length (argvals, multiFunData)
  expect_error(setX(m, list(getX(f1), getX(f2), matrix(1:12, nrow = 4))), 'setX: multiFunData object and newX must have the same length') # wrong length (X, multiFunData)
  expect_error(setX(m, list(matrix(1:25, nrow = 5), array(1:120, c(4,5,6)))), 'setX: newX object must have the same number of observations in all elements!') # different number of observations
  expect_warning(setX(m, list(matrix(1:25, nrow = 5), array(1:150, c(5,5,6)))), 'setX: Number of observations has changed') # warning: more observations
  # irreg FD object
  expect_error(setArgvals(i1, list(1:4)), "setArgvals: newArgvals must be a list of the same length as the original argvals.")
  expect_error(setArgvals(i1, list(1:6, 1:3)), "setArgvals: newArgvals must have the same structure as the original argvals.")
  expect_error(setX(i1, list(1:4)), "setX: newX must be a list of the same length as the original X.")
  expect_error(setX(i1, list(1:6, 1:3)), "setX: newX must have the same structure as the original X.")
  
  # Check functionality:
  # univariate FD object (one-dim)
  expect_equal(getArgvals(setArgvals(f1, list(1+1:5))), list(1+1:5))
  expect_equal(getArgvals(setArgvals(f1, 1+1:5)), list(1+1:5)) # special case: one-dimensional domain
  expect_equal(getX(setX(f1, matrix(1+1:20, nrow = 4))), matrix(1+1:20, nrow = 4))
  # univariate FD object (two-dim)
  expect_equal(getArgvals(setArgvals(f2, list(1+1:5, 2+1:6))), list(1+1:5, 2+1:6))  
  # multivariate FD object
  expect_equal(getArgvals(setArgvals(m, list(list(2+1:5), list(1+1:5, 3+1:6)))), list(list(2+1:5), list(1+1:5, 3+1:6)))
  expect_equal(getX(setX(m, list(matrix(1+1:20, nrow = 4), array(2+1:120, c(4,5,6))))), list(matrix(1+1:20, nrow = 4), array(2+1:120, c(4,5,6))))
  expect_equal(getArgvals(setArgvals(m, list(1+1:5, list(2+1:5, 3+1:6)))), list(list(1+1:5), list(2+1:5, 3+1:6))) # special case: one-dimensional domains
  # irreg FD object
  expect_equal(getArgvals(setArgvals(i1, list(0:4,0:2))), list(0:4, 0:2))
  expect_equal(getX(setX(i1, list(0:4,0:2))), list(0:4, 0:2))
  
  # check multivariate functions with one element
  expect_equal(getArgvals(f1), getArgvals(as.multiFunData(f1))[[1]])
})

test_that("flipFun", {
  f1 <- funData(argvals = 1:5, X = matrix(1:20, nrow = 4))
  f2 <- funData(argvals = list(1:5, 1:6), X = array(1:120,c(4,5,6)))
  m1 <- multiFunData(list(f1,f2))
  i1 <- irregFunData(argvals = list(1:5, 2:5, 3:5, c(1,3,5)), X = list(2:6, 3:6, 4:6, c(2,4,6)))
  
  # Check errors:
  # univariate FD object (one-dim)
  expect_error(flipFuns(f1,funData(argvals = list(1:5), X = array(1:30,c(6,5)))), 
               'flipFuns: Functions must have the same number of observations or use a single function as reference.') # not the same number of observations
  expect_error(flipFuns(f1,f2), 
               'flipFuns: Functions must have the dimension.') # not the same dimension
  expect_error(flipFuns(f1,funData(argvals = list(2:6), X = array(1:20,c(4,5)))), 
               'flipFuns: Functions must be defined on the same domain.') # not the same domain
  # irreg FD object (regular reference)
  expect_error(flipFuns(f2,i1),
               "flipFuns: Function is only implemented for irregular data with one-dimensional support")
  expect_error(flipFuns(extractObs(f1, 1:2), i1),
               "flipFuns: Functions must have the same number of observations or use a single function as reference.")
  expect_error(flipFuns(extractObs(f1, argvals = 1:3), i1),
               "flipFuns: Irregular functions must be defined on a sub-domain of the reference function(s).", fixed = TRUE)# fixed, as '(...)' is interpreted as regexp
  # irreg FD object (irregular reference)
  expect_error(flipFuns(extractObs(i1, 1:2), i1),
               "flipFuns: Functions must have the same number of observations or use a single function as reference.")
  expect_error(flipFuns(extractObs(i1, argvals = 1:3), i1),
               "flipFuns: New functions must be defined on a sub-domain of the reference function(s).", fixed = TRUE)  # fixed, as '(...)' is interpreted as regexp
  
  # Check functionality:
  # univariate FD object (one-dim)
  expect_equal(flipFuns(f1, -1*f1), f1)
  # univariate FD object (two-dim)
  expect_equal(flipFuns(f2, -1*f2), f2)
  # multivariate FD object
  expect_equal(flipFuns(m1, -1*m1), m1) 
  # irreg FD object
  expect_equal(flipFuns(f1,i1),i1) # regular reference for each observation
  expect_equal(flipFuns(extractObs(f1, obs = 1), i1), i1) # single  regular reference function
  expect_equal(flipFuns(i1, -1*i1), i1) # irreg reference for each observation
  expect_equal(flipFuns(extractObs(i1, obs = 1), -1*i1), i1) # irreg reference for each observation
})


test_that("meanFunction",{
  x <- seq(0, 2*pi, 0.01)
  f1 <- funData(x, outer(seq(0.75, 1.25, 0.05), sin(x)))
  f1NA <- f1; f1NA@X[sample(prod(dim(f1NA@X)), 100)] <- NA
  f2 <- funData(list(1:5, 1:3), array(rep(1:5,each = 11, times = 3), dim = c(11,5,3)))
  m <- multiFunData(f1,f2)
  i1 <- irregFunData(argvals = list(1:3,1:3,1:3), X = list(1:3,2:4,3:5))
 
  # Check errors:
  # irreg FD object
  expect_error(meanFunction(irregFunData(argvals = list(1:3,1:5), X = list(1:3,1:5))),
                             "Mean function defined only for irregular functional data objects on the same domain.")
  expect_error(meanFunction(i1, na.rm = TRUE),
               "Option na.rm = TRUE is not implemented for mean functions of irregular data.")
  
  # Check functionality:
  # univariate FD object (one-dim)
  expect_equal(meanFunction(f1), extractObs(f1, obs = 6))
  expect_equal(meanFunction(f1NA), funData(f1NA@argvals, array(colMeans(f1NA@X), dim = c(1, length(x)))))
  # univariate FD object (two-dim)
  expect_equal(meanFunction(f2), extractObs(f2, obs = 6)) # or any other observation
  # multivariate FD object
  expect_equal(meanFunction(m), extractObs(m, obs = 6))
  # irregular FD object
  expect_equal(meanFunction(i1), extractObs(i1, 2))
 })

test_that("expand.int",{
  expect_null(expand.int())
  expect_equal(expand.int(2,5), data.frame(Var1 = rep(1:2, each = 5), Var2 = rep(1:5, times = 2)))
}
          )

test_that("tensorProduct",{
  x <- seq(0, 2*pi, 0.1)
  f1 <- funData(x, outer(seq(0.75, 1.25, 0.1), sin(x)))
  y <- seq(-pi, pi, 0.1)
  f2 <- funData(y, outer(seq(0.25, 0.75, 0.1), sin(y)))
  
  # Check errors:
 expect_error(tensorProduct(f1), "tensorProduct currently accepts only 2 or 3 arguments.")
 expect_error(tensorProduct(f1, f2, f2, f1), "tensorProduct currently accepts only 2 or 3 arguments.")
 expect_error(tensorProduct(f1, tensorProduct(f1,f2)), "tensorProduct is defined only for funData objects on one-dimensional domains!")
 
 # Check functionality:
 # tensor product of two functions
 TP1 <- tensorProduct(f1, f2)
 expect_equal(dimSupp(TP1), 2)
 expect_equal(TP1@argvals, list(f1@argvals[[1]], f2@argvals[[1]]))
 expect_equal(nObs(TP1), nObs(f1)*nObs(f2))
 expect_equal(mean(TP1@X[1,-1,]/TP1@X[7,-1,]), 0.88235294) # what was 2nd before now is 7th
 expect_equal(sum(var(TP1@X[1,-1,]/TP1@X[7,-1,])), 0)
 
 # tensor product of three functions
 TP2 <- tensorProduct(f1, f2, f1)
 expect_equal(dimSupp(TP2), 3)
 expect_equal(TP2@argvals, list(f1@argvals[[1]], f2@argvals[[1]], f1@argvals[[1]]))
 expect_equal(nObs(TP2), nObs(f1)^2*nObs(f2))
 expect_equal(mean(TP2@X[1,-1,-1,-1]/TP2@X[2,-1,-1,-1]), 0.88235294)
 expect_equal(sum(var(TP2@X[1,-1,-1,-1]/TP2@X[2,-1,-1,-1])), 0)
 })

test_that("approxNA",{
  x <- seq(0, 2*pi, 0.1)
  f1 <- funData(x, outer(seq(0.75, 1.25, 0.1), x)) # linear functions
  
  set.seed(1)
  expect_equal(integrate(f1 - as.irregFunData(approxNA(sparsify(f1, minObs = 5, maxObs = 8)))),
               rep(0, nObs(f1)))
  
})
