context("Test funData methods")

f1 <- funData(argvals = 1:5, X = matrix(1:20, nrow = 4))
f2 <- funData(argvals = list(1:5, 1:6), X = array(1:120, c(4,5,6)))
f3 <- funData(argvals = list(1:5, 1:6, 1:4), X = array(1:480, c(4, 5, 6, 4)))
m1 <- multiFunData(f1, f2)
i1 <- irregFunData(argvals = list(1:5, 2:4, 3:5), X = list(1:5, 2:4, -(3:1)))
fi <- as.irregFunData(f1)

# special case for data with only one observation
f1.1 <- funData(argvals = 1:5, X = matrix(1:5, nrow = 1))
f2.1 <- funData(argvals = list(1:5, 1:6), X = array(1:30,c(1,5,6)))
m1.1 <- multiFunData(list(f1.1,f2.1))

test_that("print",{
  expect_known_output(print(f1), file = "outputs/print_funData.out")
  expect_known_output(print(m1), file = "outputs/print_multiFunData.out")
  expect_known_output(print(i1), file = "outputs/print_irregFunData.out")
})

test_that("str",{
  expect_known_output(str(f1), file = "outputs/str_funData.out")
  expect_known_output(str(m1), file = "outputs/str_multiFunData.out")
  expect_known_output(str(i1), file = "outputs/str_irregFunData.out")
  expect_known_output(str(i1, list.len = 1), file = "outputs/str_irregFunData_len1.out")
})

test_that("summary",{
  # Check errors:
  expect_error(funData:::print.summary.funData(summary(m1)), 
               "Argument is not of class 'summary.funData'.")
  expect_error(funData:::print.summary.multiFunData(summary(f1)), 
               "Argument is not of class 'summary.multiFunData'.")
  expect_error(funData:::print.summary.irregFunData(summary(f1)), 
               "Argument is not of class 'summary.irregFunData'.")
  
  # Check functionality:
  expect_known_output(print(summary(f1)), file = "outputs/summary_funData.out")
  expect_known_output(print(summary(m1)), file = "outputs/summary_multiFunData.out")
  expect_known_output(print(summary(i1)), file = "outputs/summary_irregFunData.out")
})

test_that("names",{
  # Check errors:
  expect_error(names(f1) <- letters[1:3], "Names must have the same length as funData object.")
  expect_error(names(m1) <- letters[1:3], "Names must have the same length as multiFunData object.")
  expect_error(names(i1) <- letters[1:5], "Names must have the same length as irregFunData object.")
  
  # Check functionality:
  # funData (1D)
  names1 <- paste("Obs", 1:4)
  expect_equal({names(f1) <- names1}, names1) 
  expect_equal(names(f1), names1)
  # multiFunData
  namesM <- paste("Element", 1:2)
  expect_equal({names(m1) <- namesM}, namesM) 
  expect_equal(names(m1), namesM)
  # irregFunData
  namesI <- paste("Obs", 1:3)
  expect_equal({names(i1) <- namesI}, namesI)
  expect_equal(names(i1), names(i1@argvals))
  expect_equal(names(i1), names(i1@X))
})

test_that("dimSupp", {
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
  # Check functionality:
  # univariate FD object (one-dim)
  expect_equal(nObs(f1), 4)
  # univariate FD object (two-dim)
  expect_equal(nObs(f2), 4)
  # multivariate FD object
  expect_equal(nObs(m1), 4)
  # irreg FD object
  expect_equal(nObs(i1),3)
})


test_that("nObsPoints", {
  # Check functionality:
  # univariate FD object (one-dim)
  expect_equal(nObsPoints(f1), 5)
  # univariate FD object (two-dim)
  expect_equal(nObsPoints(f2), c(5,6))
  # multivariate FD object
  expect_equal(nObsPoints(m1), list(5,c(5,6)))
  # irreg FD object
  expect_equal(nObsPoints(i1), c(5,3,3))  
})

test_that("extractObs", {
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
  expect_error(extractObs(i1, obs = 4),
               "Trying to extract observations that do not exist!")
  expect_error(extractObs(extractObs(i1, argvals = "1")),
               "Supply argvals for extracted observations either as list or as numeric vector")
  expect_error(extractObs(i1, argvals = 6),
               "Trying to extract x-values that do not exist!")
  expect_warning(extractObs(i1, argvals = c(1,5)),
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
  expect_equal(extractObs(i1, argvals = list(3:4)), extractObs(i1, argvals = 3:4))
  expect_equal(extractObs(i1, obs = 1), irregFunData(argvals = list(1:5), X = list(1:5)))
  expect_equal(extractObs(i1, argvals = 2:3), irregFunData(argvals = list(2:3, 2:3, 3), X = list(2:3, 2:3, -3)))
})

test_that("Arith", {
  # Check errors:
  # univariateFD, univariate FD
  expect_error(f1 + extractObs(f1,1:2), 
               "nObs of funData objects is neither equal nor one.")
  
  # irreg & irreg
  expect_error(extractObs(i1, obs = 2) + i1,
               "Multiple functions must be defined on subdomain of single function.")
  expect_error(i1 + extractObs(i1, obs = 2),
               "Multiple functions must be defined on subdomain of single function.")
  expect_error(i1 + extractObs(i1, obs = 1:2),
               "IrregFunData objects must have either the same number of observations or just one.")
  expect_error(i1 +  irregFunData(argvals = lapply(i1@argvals, function(l){l+1}), X = i1@X),
               "Arithmetics for two irregular functional data objects are defined only for functions on the same domain.")
  
  # irreg & reg
  expect_error(i1 + extractObs(f1, argvals = 3:4, obs = 1:3),
               "irregFunData object must be defined on a subdomain of the funData object!")
  expect_error(i1+f1,
               "funData object must have either one observation or the same number of observations as the irregFunData object")
  
  
  # Check functionality:
  # univariate & univariate
  expect_equal(f1+f1, funData(f1@argvals,f1@X+f1@X))
  expect_equal(f1-f1, funData(f1@argvals,f1@X-f1@X))
  expect_equal(f1*f1, funData(f1@argvals,f1@X*f1@X))
  expect_equal(f1/f1, funData(f1@argvals,f1@X/f1@X))  
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
  x1 <- unique(unlist(i1@argvals))
  expect_equal(i1+i1, irregFunData(i1@argvals,mapply('+', i1@X, i1@X)))
  expect_equal(i1-i1, irregFunData(i1@argvals,mapply('-', i1@X, i1@X)))
  expect_equal(i1*i1, irregFunData(i1@argvals,mapply('*', i1@X, i1@X)))
  expect_equal(i1/i1, irregFunData(i1@argvals,mapply('/', i1@X, i1@X)))
  expect_equal(i1 + irregFunData(argvals = list(x1), X = list(rep(0, length(x1)))), i1)
  expect_equal(irregFunData(argvals = list(x1), X = list(rep(1, length(x1)))) + i1, 1+ i1)
  # irreg & reg
  expect_equal(i1 + extractObs(f1, obs = 1:3), extractObs(f1, obs = 1:3) + i1) # same number of observations
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
  # Check functionality:
  # funData
  expect_equal(exp(f1), funData(f1@argvals, exp(f1@X)))
  expect_equal(sin(f1)^2 + cos(f1)^2, 0*f1+1) # combination of Arith and math
  
  # irregFunData
  expect_equal(exp(i1), irregFunData(i1@argvals, lapply(i1@X,exp)))
  expect_equal(sin(i1)^2 + cos(i1)^2, 0*i1+1) # combination of Arith and math
  
  # multiFunData
  expect_equal(exp(m1), multiFunData(exp(f1), exp(f2)))
  expect_equal(sin(m1)^2 + cos(m1)^2, 0*m1+1) # combination of Arith and math
})

test_that("norm", {
  # Check errors:
  # univariate FD object
  expect_error(norm(f1, squared = "Yes"), "Parameter 'squared' must be passed as a logical.") 
  expect_error(norm(f1, squared = c(TRUE, FALSE)), "Parameter 'squared' must be passed as a logical.") 
  expect_error(norm(f1, weight = "1"), "Parameter 'weight' must be passed as a positive number.")
  expect_error(norm(f1, weight = 1:2), "Parameter 'weight' must be passed as a positive number.")
  expect_error(norm(f1, weight = -1), "Parameter 'weight' must be passed as a positive number.")
  # multivariate FD object
  expect_error(norm(m1, weight = c(1,"2")),
               "Parameter 'weight' must be passed as a vector of 2 positive numbers.") 
  expect_error(norm(m1, weight = 1:3),
               "Parameter 'weight' must be passed as a vector of 2 positive numbers.") 
  expect_error(norm(m1, weight = c(-1,1)),
               "Parameter 'weight' must be passed as a vector of 2 positive numbers.") 
  # irreg FD object
  expect_error(norm(i1, squared = "Yes"), "Parameter 'squared' must be passed as a logical.")
  expect_error(norm(i1, squared = c(TRUE, TRUE)), "Parameter 'squared' must be passed as a logical.")
  expect_error(norm(i1, weight = "1"), "Parameter 'weight' must be passed as a positive number.")
  expect_error(norm(i1, weight = 1:2), "Parameter 'weight' must be passed as a positive number.")
  expect_error(norm(i1, weight = -1), "Parameter 'weight' must be passed as a positive number.")
  
  # Check functionality:
  # univariate FD object
  expect_equal(norm(f1), # all observations
               apply((f1^2)@X, 1, function(f, argvals, method){funData:::.intWeights(argvals, method) %*% f}, argvals = f1@argvals[[1]], method = "trapezoidal") )
  expect_equal(norm(f1)[1:3], norm(f1, obs = 1:3))   # only some observations
  expect_equal(norm(f1, squared = FALSE)[2], sqrt(norm(f1)[2])) # squared option
  expect_equal(norm(f1, weight = 2), 2*norm(f1)) # weight (makes little sense for univariate funData objects...)
  # multivariate FD object
  expect_equal(norm(m1), rowSums(sapply(m1, norm, simplify = TRUE))) # all observations
  expect_equal(norm(m1)[1], norm(m1, obs = 1)) # only one observation
  expect_equal(norm(m1, squared = FALSE), sqrt(rowSums(sapply(m1, norm, squared = TRUE, simplify = TRUE)))) # squared option
  expect_equal(norm(m1, weight = c(2,1)), norm(multiFunData(sqrt(2)*f1,f2))) # with weight
  # irreg FD object  
  expect_equal(norm(i1), c(42,19,9), tolerance = 1e-5) # result calculated explicitly
  expect_equal(norm(i1, fullDom = TRUE), c(42,42,43), tolerance = 1e-5) # result calculated explicitly
  expect_equal(norm(i1, weight = 2), 2*norm(i1)) # weight (makes little sense for univariate funData objects...)
})


test_that("scalarProduct", {
  # Check errors:
  expect_error(scalarProduct(m1, as.multiFunData(f1)),
               "multiFunData objects must have the same number of elements.")
  expect_error(scalarProduct(m1, m1, weight = c(-1,1)),
               "Weights must be non-negative.")
  expect_error(scalarProduct(m1, m1, weight = c(0,0)),
               "At least one weighting factor must be different from 0.")
  
  # Check functionality:
  # univariate FD objects
  s <- scalarProduct(f1, 2*f1)
  expect_equal(length(s), nObs(f1))
  expect_equal(s[1], 840, tol = 1e-5)
  expect_equal(scalarProduct(f1,f1), norm(f1, squared = TRUE))
  # multivariate FD object
  expect_equal(scalarProduct(m1,m1), norm(m1, squared = TRUE))
  expect_equal(scalarProduct(m1,m1, weight = c(1,2)), norm(m1, squared = TRUE, weight = c(1,2))) # with weights
  # irreg FD object  
  expect_equal(scalarProduct(i1,i1), norm(i1, squared = TRUE))
})

test_that("integrate", {
  # Check errors:  
  expect_error(integrate(funData(argvals = list(1:2,1:3,1:4,1:5), X = array(rnorm(120), dim = c(1,2,3,4,5)))),
               "Integration is not yet defined for functional data objects with dim > 3")
  expect_error(integrate(f1, method = 1),"Parameter 'method' must be a string.")
  expect_error(integrate(f1, method = c("m1", "m2")),"Parameter 'method' must be a string.")
  expect_error(integrate(i1, fullDom = "Yes"),"Parameter 'fullDom' must be a logical.") 
  expect_error(integrate(i1, fullDom = c(TRUE, FALSE)),"Parameter 'fullDom' must be a logical.")
  expect_warning(integrate(extractObs(f1, argvals = 1:2)), # method = trapezoidal and not enough observation points
                 "Trapezoidal quadrature is not applicable for functions with < 3 observation points. 'method' changed to 'midpoint'.")
  
  # Check functionality:
  # univariate FD objects
  expect_equal(integrate(f1)[1], sum(funData:::.intWeights(f1@argvals[[1]], "trapezoidal")*f1@X[1,]))
  expect_equal(integrate(f2)[1], as.numeric(t(funData:::.intWeights(f2@argvals[[1]], "trapezoidal")) %*% 
                                              f2@X[1,,] %*% funData:::.intWeights(f2@argvals[[2]], "trapezoidal")))
  # multivariate FD objects
  expect_equal(integrate(m1), as.numeric(integrate(f1) + integrate(f2)))
  expect_equal(integrate(m1.1), as.numeric(integrate(f1.1) + integrate(f2.1)))
  # irreg FD object 
  expect_equal(integrate(i1), c(12,6,-4), tolerance = 1e-5)
  expect_equal(integrate(i1, fullDom = TRUE), c(12,12,-12), tolerance = 1e-5)
})

test_that("integrate3D",{
  x <- seq(0,1, 0.02); nX <- length(x)
  y <- seq(-0.5,0.5, 0.02); nY <- length(y)
  z <- seq(1,2,0.02); nZ <- length(z)
  
  A <- array(NA, c(nX, nY, nZ))
  
  for(ix in 1:nX)
    for(iy in 1:nY)
      for(iz in 1:nZ)
        A[ix,iy,iz] <-  x[ix]*cos(pi*y[iy])*z[iz]^2
  
  expect_equal(funData:::integrate3D(A, argvals = list(x,y,z)), 7/(3*pi), tolerance = 1e-3)
})

test_that("set/get", {
  # Check errors:
  # univariate FD object (one-dim)
  expect_error(setArgvals(f1, 1:6), 
               "argvals and X have different number of sampling points! X-Dimensions must be of the form N x M1 x ... x Md") # wrong number of sampling points (argvals)
  expect_error(setX(f1, matrix(1:24, nrow = 4)), 
               "argvals and X have different number of sampling points! X-Dimensions must be of the form N x M1 x ... x Md") # wrong number of sampling points (X)
  expect_warning(setX(f1, matrix(1:25, nrow = 5)), 'Number of observations has changed') # warning: more observations
  # univariate FD object (two-dim)
  expect_error(setArgvals(f2, 1:5), 
               "argvals and X element have different support dimensions! X-Dimensions must be of the form N x M1 x ... x Md") # wrong dimension (argvals)
  expect_error(setX(f2, matrix(1:20, nrow = 4)), 
               "argvals and X element have different support dimensions! X-Dimensions must be of the form N x M1 x ... x Md") # wrong dimension (X)
  # multivariate FD object
  expect_error(setArgvals(m1, list(1+1:5, list(2+1:5, 3+1:6), 4+1:5)), 'multiFunData object and newArgvals must have the same length') # wrong length (argvals, multiFunData)
  expect_error(setX(m1, list(getX(f1), getX(f2), matrix(1:12, nrow = 4))), 'multiFunData object and newX must have the same length') # wrong length (X, multiFunData)
  expect_error(setX(m1, list(matrix(1:25, nrow = 5), array(1:120, c(4,5,6)))), 'newX object must have the same number of observations in all elements!') # different number of observations
  expect_warning(setX(m1, list(matrix(1:25, nrow = 5), array(1:150, c(5,5,6)))), 'Number of observations has changed') # warning: more observations
  # irreg FD object
  expect_error(setArgvals(i1, list(1:4)), "newArgvals must be a list of the same length as the original argvals.")
  expect_error(setArgvals(i1, list(1:6, 1:3, 1:10)), "newArgvals must have the same structure as the original argvals.")
  expect_error(setX(i1, list(1:4)), "newX must be a list of the same length as the original X.")
  expect_error(setX(i1, list(1:6, 1:3, 1:10)), "newX must have the same structure as the original X.")
  
  # Check functionality:
  # univariate FD object (one-dim)
  expect_equal(getArgvals(setArgvals(f1, list(1+1:5))), list(1+1:5))
  expect_equal(getArgvals(setArgvals(f1, 1+1:5)), list(1+1:5)) # special case: one-dimensional domain
  expect_equal(getX(setX(f1, matrix(1+1:20, nrow = 4))), matrix(1+1:20, nrow = 4))
  # univariate FD object (two-dim)
  expect_equal(getArgvals(setArgvals(f2, list(1+1:5, 2+1:6))), list(1+1:5, 2+1:6))  
  # multivariate FD object
  expect_equal(getArgvals(setArgvals(m1, list(list(2+1:5), list(1+1:5, 3+1:6)))), list(list(2+1:5), list(1+1:5, 3+1:6)))
  expect_equal(getX(setX(m1, list(matrix(1+1:20, nrow = 4), array(2+1:120, c(4,5,6))))), list(matrix(1+1:20, nrow = 4), array(2+1:120, c(4,5,6))))
  expect_equal(getArgvals(setArgvals(m1, list(1+1:5, list(2+1:5, 3+1:6)))), list(list(1+1:5), list(2+1:5, 3+1:6))) # special case: one-dimensional domains
  # irreg FD object
  expect_equal(getArgvals(setArgvals(i1, list(0:4, 0:2, 1:3))), list(0:4, 0:2, 1:3))
  expect_equal(getX(setX(i1, list(0:4, 0:2, 1:3))), list(0:4, 0:2, 1:3))
  
  # check multivariate functions with one element
  expect_equal(getArgvals(f1), getArgvals(as.multiFunData(f1))[[1]])
  
  # check aliases
  expect_equal(getArgvals(f1), argvals(f1))
  expect_equal(getArgvals(f2), argvals(f2))
  expect_equal(getArgvals(m1), argvals(m1))
  expect_equal(getArgvals(i1), argvals(i1))
  
  expect_equal(getX(f1), X(f1))
  expect_equal(getX(f2), X(f2))
  expect_equal(getX(m1), X(m1))
  expect_equal(getX(i1), X(i1))
  
})

test_that("flipFun", {
  # Check errors:
  # univariate FD object (one-dim)
  expect_error(flipFuns(f1,funData(argvals = list(1:5), X = array(1:30,c(6,5)))), 
               'Functions must have the same number of observations or use a single function as reference.') # not the same number of observations
  expect_error(flipFuns(f1,f2), 
               'Functions must have the dimension.') # not the same dimension
  expect_error(flipFuns(f1,funData(argvals = list(2:6), X = array(1:20,c(4,5)))), 
               'Functions must be defined on the same domain.') # not the same domain
  # irreg FD object (regular reference)
  expect_error(flipFuns(f2,i1),
               "Function is only implemented for irregular data with one-dimensional support")
  expect_error(flipFuns(extractObs(f1, 1:2), i1),
               "Functions must have the same number of observations or use a single function as reference.")
  expect_error(flipFuns(extractObs(f1, argvals = 1:3), fi),
               "Irregular functions must be defined on a sub-domain of the reference function(s).", fixed = TRUE)# fixed, as '(...)' is interpreted as regexp
  # irreg FD object (irregular reference)
  expect_error(flipFuns(extractObs(i1, 1:2), i1),
               "Functions must have the same number of observations or use a single function as reference.")
  expect_error(flipFuns(extractObs(i1, argvals = 1:3), i1),
               "New functions must be defined on a sub-domain of the reference function(s).", fixed = TRUE)  # fixed, as '(...)' is interpreted as regexp
  
  # Check functionality:
  # univariate FD object (one-dim)
  expect_equal(flipFuns(f1, -1*f1), f1)
  # univariate FD object (two-dim)
  expect_equal(flipFuns(f2, -1*f2), f2)
  # multivariate FD object
  expect_equal(flipFuns(m1, -1*m1), m1) 
  # irreg FD object
  expect_equal(flipFuns(f1,fi),fi) # regular reference for each observation
  expect_equal(flipFuns(extractObs(f1, obs = 1), fi), fi) # single  regular reference function
  expect_equal(flipFuns(i1, -1*i1), i1) # irreg reference for each observation
  expect_equal(flipFuns(extractObs(fi, obs = 1), -1*fi), fi) # irreg reference for each observation
})


test_that("meanFunction",{
  set.seed(2)
  f1NA <- f1; f1NA@X[sample(prod(dim(f1NA@X)), 5)] <- NA
  
  # Check errors:
  # funData
  expect_error(meanFunction(f1, na.rm = "Yes"), "Parameter 'na.rm' must be a logical.")
  expect_error(meanFunction(f1, na.rm = c(TRUE, FALSE)), "Parameter 'na.rm' must be a logical.")
  # irreg FD object
  expect_error(meanFunction(i1, na.rm = "Yes"), "Parameter 'na.rm' must be a logical.")
  expect_error(meanFunction(i1, na.rm = c(TRUE, FALSE)), "Parameter 'na.rm' must be a logical.")
  expect_error(meanFunction(irregFunData(argvals = list(1:3,1:5), X = list(1:3,1:5))),
               "Mean function defined only for irregular functional data objects on the same domain.")
  expect_error(meanFunction(i1, na.rm = TRUE),
               "Option na.rm = TRUE is not implemented for mean functions of irregular data.")
  
  # Check functionality:
  # univariate FD object (one-dim)
  expect_equal(meanFunction(f1), {mean1 <- funData(1:5, matrix(seq(2.5, 18.5, 4), nrow = 1))})
  expect_equal(meanFunction(f1NA), 
               funData(f1NA@argvals, array(colMeans(f1NA@X), dim = c(1, length(f1@argvals[[1]])))))
  # univariate FD object (two-dim)
  expect_equal(meanFunction(f2), {mean2 <- funData(f2@argvals, array(seq(2.5,118.5, 4), dim = c(1,5,6)))})
  # multivariate FD object
  expect_equal(meanFunction(m1), multiFunData(mean1,mean2))
  # irregular FD object
  expect_equal(meanFunction(fi), as.irregFunData(mean1))
})

test_that("expand.int",{
  expect_null(funData:::expand.int())
  expect_equal(funData:::expand.int(2,5), data.frame(Var1 = rep(1:2, each = 5), Var2 = rep(1:5, times = 2)))
}
)

test_that("tensorProduct",{
  # Check errors:
  expect_error(tensorProduct(f1), "tensorProduct currently accepts only 2 or 3 arguments.")
  expect_error(tensorProduct(f1, f2, f2, f1), "tensorProduct currently accepts only 2 or 3 arguments.")
  expect_error(tensorProduct(f1, tensorProduct(f1,f2)), "tensorProduct is defined only for funData objects on one-dimensional domains!")
  
  # Check functionality:
  # tensor product of two functions
  TP1 <- tensorProduct(f1, f1.1)
  expect_equal(dimSupp(TP1), 2)
  expect_equal(TP1@argvals, list(f1@argvals[[1]], f1.1@argvals[[1]]))
  expect_equal(nObs(TP1), nObs(f1)*nObs(f1.1))
  expect_equal(apply(TP1@X[1,-1,]/TP1@X[4,-1,], 1, mean), c(0.625, 0.75, 0.8125, 0.85), tol = 1e-5)
  expect_equal(apply(TP1@X[1,-1,]/TP1@X[4,-1,], 1, var), rep(0,4), tol = 1e-5)
  
  # tensor product of three functions
  TP2 <- tensorProduct(f1, f1.1, f1)
  expect_equal(dimSupp(TP2), 3)
  expect_equal(TP2@argvals, list(f1@argvals[[1]], f1.1@argvals[[1]], f1@argvals[[1]]))
  expect_equal(nObs(TP2), nObs(f1)^2*nObs(f1.1))
  expect_equal(mean(TP2@X[1,-1,-1,-1]/TP2@X[2,-1,-1,-1]), 0.90159, tol = 1e-5)
  expect_equal(var(TP2@X[1,-1,-1,-1]/TP2@X[2,-1,-1,-1]), 0.0018352, tol = 1e-7)
})

test_that("approxNA",{
  set.seed(2)
  expect_equal(integrate(f1 - as.irregFunData(approxNA(sparsify(f1, minObs = 3, maxObs = 5)))),
               rep(0, nObs(f1)))
  
})