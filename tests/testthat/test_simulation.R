context("Test simulation methods")

test_that("eFuns", {
  argvals <- seq(0,1,0.01)
  
  # check special cases for M = 1
  expect_equal(efPoly(M = 1, argvals = argvals), 
               extractObs(efPoly(M = 4, argvals = argvals), obs = 1))
  expect_equal(efFourier(M = 1, argvals = argvals), 
               extractObs(efFourier(M = 4, argvals = argvals), obs = 1))
  
  # linear version of Fourier
  expect_error(efFourier(M = 2, argvals = argvals + 1, linear = TRUE),
               "efFourier, option linear: not yet implemented for argvals != [0,1]!", fixed = TRUE)
  expect_equal(norm(efFourier(M = 4, argvals = argvals, linear = TRUE)), 
               c(1, 1, 1, 1.0015304))
  
  # eFun /eVal
  expect_error(eFun(argvals = "argvals", M = 2, type = "Poly"), "Parameter 'argvals' must be numeric.")
  expect_error(eFun(argvals = numeric(0), M = 2, type = "Poly"), "Parameter 'argvals' must be numeric.")
  expect_error(eFun(argvals = argvals, M = "2", type = "Poly"), "Parameter 'M' must be passed as a positive number.")
  expect_error(eFun(argvals = argvals, M = 1:2, type = "Poly"), "Parameter 'M' must be passed as a positive number.")
  expect_error(eFun(argvals = argvals, M = -2, type = "Poly"), "Parameter 'M' must be passed as a positive number.")
  expect_error(eFun(argvals = argvals, M = 2, ignoreDeg = "1", type = "PolyHigh"),
               "Parameter 'ignoreDeg' must be either NULL or a vector of positive numbers.")
  expect_error(eFun(argvals = argvals, M = 2, ignoreDeg = -2, type = "PolyHigh"),
               "Parameter 'ignoreDeg' must be either NULL or a vector of positive numbers.")
  expect_error(eFun(argvals = argvals, M = 2, type = 2),
               "Parameter 'type' must be passed as a string.")
  expect_error(eFun(argvals = argvals, M = 2, type = c("Poly", "Fourier")),
               "Parameter 'type' must be passed as a string.")
  expect_error(eVal(M = "2", type = "linear"),
               "Parameter 'M' must be passed as a positive number.")
  expect_error(eVal(M = 1:2, type = "linear"),
               "Parameter 'M' must be passed as a positive number.")
  expect_error(eVal(M = -2, type = "linear"),
               "Parameter 'M' must be passed as a positive number.")
  expect_error(eVal(M = 2, type = 1),
               "Parameter 'type' must be passed as a string.")
  expect_error(eVal(M = 2, type = c("linear", "wiener")),
               "Parameter 'type' must be passed as a string.")
  
  expect_error(eFun(argvals = argvals, M = 2, type = "PolyHigh"), "eFun, type = PolyHigh: specify ignoreDeg !", fixed = TRUE)
  expect_equal(eFun(argvals = argvals, M = 2, ignoreDeg = 1:2, type = "PolyHigh"),
               extractObs(eFun(argvals = argvals, M = 4, type = "Poly"), obs = 3:4))
})

test_that("simFunData",{
  # check errors
  expect_error(simFunData(argvals = seq(0,1,0.01), M = "10", eFunType = "Fourier", eValType = "linear", N = 4),
               "Parameter 'M' must be numeric.") 
  expect_error(simFunData(argvals = 1:10, M = c(10,20), eFunType = "Fourier", eValType = "linear", N = 4),
               "M must have the same length as argvals or 1.")
  expect_error(simFunData(argvals = seq(0,1,0.01), M = 10, eFunType = 1, eValType = "linear", N = 4),
               "Parameter 'eFunType' must be passed as a string.")
  expect_error(simFunData(argvals = 1:10, M = 4, eFunType = c("Fourier", "Poly"), eValType = "linear", N = 4),
               "eFunType must have the same length as argvals or 1.")
  expect_error(simFunData(argvals = seq(0,1,0.01), M = 10, eFunType = "Fourier", eValType = "linear", ignoreDeg = "1", N = 4),
               "Parameter 'ignoreDeg' must be either NULL or a vector of positive numbers.")
  expect_error(simFunData(argvals = seq(0,1,0.01), M = 10, eFunType = "Fourier", eValType = "linear", ignoreDeg = -1, N = 4),
               "Parameter 'ignoreDeg' must be either NULL or a vector of positive numbers.")
  expect_error(simFunData(argvals = seq(0,1,0.01), M = 10, eFunType = "Fourier", eValType = 1, N = 4),
               "Parameter 'eValType' must be passed as a string.")
  expect_error(simFunData(argvals = seq(0,1,0.01), M = 10, eFunType = "Fourier", eValType = c("linear", "wiener"), N = 4),
               "Parameter 'eValType' must be passed as a string.")
  expect_error(simFunData(argvals = seq(0,1,0.01), M = 10, eFunType = "Fourier", eValType = "linear", N = "4"),
               "Parameter 'N' must be passed as a positive number.")
  expect_error(simFunData(argvals = seq(0,1,0.01), M = 10, eFunType = "Fourier", eValType = "linear", N = 1:4),
               "Parameter 'N' must be passed as a positive number.")
  expect_error(simFunData(argvals = seq(0,1,0.01), M = 10, eFunType = "Fourier", eValType = "linear", N = -4),
               "Parameter 'N' must be passed as a positive number.")
  expect_error(simFunData(argvals = rep("argvals",4), M = 10, eFunType = "Fourier", eValType = "linear", N = 4),
               "Parameter 'argvals' must be either passed as a list or as a vector of numerics.")
  expect_error(simFunData(argvals = list("argvals",1:4), M = 10, eFunType = "Fourier", eValType = "linear", N = 4),
               "Parameter 'argvals' must be either passed as a list or as a vector of numerics.")
  
  # check warnings
  expect_warning(simFunData(argvals = list(seq(0,1,0.01), seq(-pi/2, pi/2, 0.02)), M = 5, eFunType = c("Poly","Fourier"), eValType = "linear", N = 4),
                 "Simulation of tensor product data. The value of M will be used for all dimensions.")
  expect_warning(simFunData(argvals = list(seq(0,1,0.01), seq(-pi/2, pi/2, 0.02)), M = c(5,8), eFunType = "Fourier", eValType = "linear", N = 4),
                 "Simulation of tensor product data. The value of eFunType will be used for all dimensions.")
  
  # check functionality:
  
  # one-dimensional domains
  set.seed(1)
  f <- simFunData(argvals = seq(0,1,0.01), M = 10, eFunType = "Fourier", eValType = "linear", N = 4)
  expect_equal(nObs(f$trueFuns), 10)
  expect_equal(norm(f$trueFuns)[1], 1)
  expect_equal(nObs(f$simData), 4)
  expect_equal(norm(f$simData)[1], 3.49879378)
  
  # tensor product eigenfunctions
  set.seed(2)
  f2 <- simFunData(argvals = list(seq(0,1,0.01), seq(-pi/2, pi/2, 0.02)), M = c(5,8), eFunType = c("Poly","Fourier"), eValType = "linear", N = 4)
  
  expect_equal(nObs(f2$trueFuns), 40)
  expect_equal(norm(f2$trueFuns)[1], 1)
  expect_equal(nObs(f2$simData), 4)
  expect_equal(norm(f2$simData)[1], 25.4940667)
})

test_that("simMultiFunData", {
  # check errors
 expect_error(simMultiFunData(type = 1, argvals = list(seq(0,1,0.01), seq(-0.5,0.5,0.02)),
                              M = 5, eFunType = "Poly", eValType = "linear", N = 7),
               "Parameter 'type' must be passed as a string.")
  expect_error(simMultiFunData(type = c("split", "weighted"), argvals = list(seq(0,1,0.01), seq(-0.5,0.5,0.02)),
                               M = 5, eFunType = "Poly", eValType = "linear", N = 7),
               "Parameter 'type' must be passed as a string.")
expect_error(simMultiFunData(type = "split", argvals = seq(-0.5,0.5,0.02),
                    M = 5, eFunType = "Poly", eValType = "linear", N = 7),
    "Parameter 'argvals' must be passed as a list of numerics.")
expect_error(simMultiFunData(type = "split", argvals = list("argvals", seq(-0.5,0.5,0.02)),
                             M = 5, eFunType = "Poly", eValType = "linear", N = 7),
             "Parameter 'argvals' must be passed as a list of numerics.")
expect_error(simMultiFunData(type = "split", argvals = list(seq(0,1,0.01), seq(-0.5,0.5,0.02)),
                             M = "5", eFunType = "Poly", eValType = "linear", N = 7),
    "Parameter 'M' must contain only numerics.") 
expect_error(simMultiFunData(type = "split", argvals = list(seq(0,1,0.01), seq(-0.5,0.5,0.02)),
                             M = 5, eFunType = 1, eValType = "linear", N = 7),
             "Parameter 'eFunType' must contain only strings.")
expect_error(simMultiFunData(type = "split", argvals = list(seq(0,1,0.01), seq(-0.5,0.5,0.02)),
                             M = 5, eFunType = "PolyHigh", ignoreDeg = "2", eValType = "linear", N = 7),
                  "Parameter 'ignoreDeg' must be either NULL or a vector of positive numbers.")
expect_error(simMultiFunData(type = "split", argvals = list(seq(0,1,0.01), seq(-0.5,0.5,0.02)),
                             M = 5, eFunType = "PolyHigh", ignoreDeg = -2, eValType = "linear", N = 7),
             "Parameter 'ignoreDeg' must be either NULL or a vector of positive numbers.")
expect_error(simMultiFunData(type = "split", argvals = list(seq(0,1,0.01), seq(-0.5,0.5,0.02)),
                             M = 5, eFunType = "Poly", eValType = 1, N = 7),
               "Parameter 'eValType' must be passed as a string.")
expect_error(simMultiFunData(type = "split", argvals = list(seq(0,1,0.01), seq(-0.5,0.5,0.02)),
                             M = 5, eFunType = "Poly", eValType = c("linear", "wiener"), N = 7),
             "Parameter 'eValType' must be passed as a string.")
expect_error(simMultiFunData(type = "split", argvals = list(seq(0,1,0.01), seq(-0.5,0.5,0.02)),
                             M = 5, eFunType = "Poly", eValType = "linear", N = "7"),
                  "Parameter 'N' must be passed as a positive number.")
expect_error(simMultiFunData(type = "split", argvals = list(seq(0,1,0.01), seq(-0.5,0.5,0.02)),
                             M = 5, eFunType = "Poly", eValType = "linear", N = 1:7),
             "Parameter 'N' must be passed as a positive number.")
expect_error(simMultiFunData(type = "split", argvals = list(seq(0,1,0.01), seq(-0.5,0.5,0.02)),
                             M = 5, eFunType = "Poly", eValType = "linear", N = -7),
             "Parameter 'N' must be passed as a positive number.")

  expect_error(simMultiFunData(type = "test", argvals = list(1:10, 1:20), M = 5, eFunType = "Poly", eValType = "linear", N = 7),
               "Choose either 'split' or 'weighted' for the simulation of multivariate functional data.")
  expect_error(funData:::simMultiSplit(argvals = list(1:5), M = list(5,6), eFunType = list("Fourier"), eValType = "linear"),
               "argvals, M, eFunType, eValType must all be of length 1!")
  expect_error(funData:::simMultiWeight(argvals = list(1:5, 1:6, 1:7), M = list(5,6,7), eFunType = list("Fourier"), eValType = "linear"),
               "Function simMultiWeight: method is not implemented for objects of dimension > 2!")
  expect_error(funData:::simMultiWeight(argvals = list(list(1:25), list(1:5,1:4)), M = list(8,c(4,5)), eFunType = list("Fourier"), eValType = "linear"),
               "Function simMultiWeight: basis dimensions must be equal!")
  
  # check split version
  set.seed(1)
  split <- simMultiFunData(type = "split", argvals = list(seq(0,1,0.01), seq(-0.5,0.5,0.02)),
                           M = 5, eFunType = "Poly", eValType = "linear", N = 7)
  all.equal(nObs(split$trueFuns), 5)
  all.equal(norm(split$trueFuns)[1], 1)
  all.equal(nObs(split$simData), 7)
  all.equal(norm(split$simData)[1], 2.29714788)
  
  # check weighted version for 1D data
  set.seed(2)
  weighted1D <- simMultiFunData(type = "weighted",
                                argvals = list(list(seq(0,1,0.01)), list(seq(-0.5,0.5,0.02))),
                                M = c(5,5), eFunType = c("Poly", "Fourier"), eValType = "linear", N = 7)
  all.equal(nObs(weighted1D$trueFuns), 5)
  all.equal(norm(weighted1D$trueFuns)[1], 1)
  all.equal(nObs(weighted1D$simData), 7)
  all.equal(norm(weighted1D$simData)[1], 2.82413497)
  
  # check weighted version for 1D and 2D data
  set.seed(3)
  weighted <- simMultiFunData(type = "weighted",
                              argvals = list(list(seq(0,1,0.01), seq(0,10,0.1)), list(seq(-0.5,0.5,0.01))),
                              M = list(c(5,4), 20), eFunType = list(c("Poly", "Fourier"), "Wiener"),
                              eValType = "linear", N = 7)
  all.equal(nObs(weighted$trueFuns), 20)
  all.equal(norm(weighted$trueFuns)[1], 1)
  all.equal(nObs(weighted$simData), 7)
  all.equal(norm(weighted$simData)[1], 5.98032046)
  
  # check special case for N = 1
  set.seed(4)
  split1 <- simMultiFunData(type = "split", argvals = list(seq(0,1,0.01), seq(-0.5,0.5,0.02)),
                           M = 5, eFunType = "Poly", eValType = "linear", N = 1)
  all.equal(nObs(split1$trueFuns), 5)
  all.equal(norm(split1$trueFuns)[1], 1)
  all.equal(nObs(split1$simData), 1)
  all.equal(norm(split1$simData), 2.3134751)
  
  set.seed(5)
  weighted1 <- simMultiFunData(type = "weighted", argvals = list(list(seq(0,1,0.01))),
                            M = c(5), eFunType = c("Poly"), eValType = "exponential", N = 1)
  all.equal(nObs(weighted1$trueFuns), 5)
  all.equal(norm(weighted1$trueFuns)[1], 1)
  all.equal(nObs(weighted1$simData), 1)
  all.equal(norm(weighted1$simData), 0.90397833)
  
})

test_that("sparsify",{
  # univariate functional data
  set.seed(1)
  f <- simFunData(argvals = seq(0,1, 0.01), M = 10, eFunType = "Fourier", eValType = "linear", N = 2)$simData
  
  # check errors:
  expect_error(sparsify(f, minObs = "2", maxObs = 4), "Parameter 'minObs' must be passed as a number.")
  expect_error(sparsify(f, minObs = 2, maxObs = "4"), "Parameter 'maxObs' must be passed as a number.")
  expect_error(sparsify(f, minObs = -1, maxObs = 5), "'minObs' must be a positive integer!")
  expect_error(sparsify(f, minObs = 1, maxObs = nObsPoints(f)+1), "'maxObs' must not exceed the maximal number of observations")
  expect_error(sparsify(f, minObs = 5, maxObs = 2), "'minObs' must be smaller or equal to 'maxObs'.")
  
  # check functionality:
  set.seed(2)
  s <- as.irregFunData(sparsify(f, minObs = 2, maxObs = 4))
  expect_equal(nObs(s), 2)
  expect_equal(nObsPoints(s), c(2,2))
  expect_equal(getArgvals(s), list(c(0.57, 0.70), c(0.94, 0.95)))
  expect_equal(getX(s), list(c(1.1767821, -2.7691465), c(-2.4019547, -2.3261600)))
  
  # multivariate functional data
  m <- multiFunData(funData(argvals = 1:4, X = rbind(1:4, 2:5)), funData(argvals = 1:5, X = rbind(1:5, 2:6)))
  set.seed(3)
  s <- sparsify(m, minObs = c(1, 5), maxObs = c(2, 5))
  expect_equal(as.irregFunData(s[[1]])@argvals, list(4, 2))
  expect_equal(as.irregFunData(s[[1]])@X, list(4, 3))
  expect_equal(s[[2]], m[[2]]) # actually, no sparsification here...
})

test_that("addError",{
  # univariate functional data
  set.seed(1)
  f <- simFunData(argvals = seq(0,1, 0.01), M = 10, eFunType = "Fourier", eValType = "linear", N = 2)$simData

  # check errors
  expect_error(addError(f, sd = "1/2"), "Parameter 'sd' must be passed as a positive number.")
  expect_error(addError(f, sd = 1:5), "Parameter 'sd' must be passed as a positive number.")
  expect_error(addError(f, sd = -0.5), "Parameter 'sd' must be passed as a positive number.")
  
  # check functionality:
  set.seed(2)
  e1 <- addError(f, sd = 0.5)
  expect_equal(nObs(e1), 2)
  expect_equal(nObsPoints(e1), nObsPoints(f))
  expect_equal(norm(e1), c(3.6424288, 7.7852419))
  
  # multivariate functional data
  m <- multiFunData(funData(argvals = 1:4, X = rbind(1:4, 2:5)), funData(argvals = 1:5, X = rbind(1:5, 2:6)))
  set.seed(3)
  e2 <- addError(m, sd = c(0.5, 5))
  expect_equal(nObs(e2), nObs(m))
  expect_equal(nObsPoints(e2), nObsPoints(m))
  expect_equal(norm(e2), c(61.411285, 124.458903)) 
})
