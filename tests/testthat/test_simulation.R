context("Test simulation methods")

test_that("simFunData",{
  # check errors
  expect_error(simFunData(argvals = 1:10, M = c(10,20), eFunType = "Fourier", eValType = "linear", N = 4),
               "M must have the same length as argvals or 1.")
  expect_error(simFunData(argvals = 1:10, M = 4, eFunType = c("Fourier", "Poly"), eValType = "linear", N = 4),
               "eFunType must have the same length as argvals or 1.")
  
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
  expect_error(simMultiFunData(type = "test", argvals = list(1:10, 1:20), M = 5, eFunType = "Poly", eValType = "linear", N = 7),
               "simMultiFunData: choose either 'split' or 'weighted' for the simulation of multivariate functional data.")
  expect_error(funData:::simMultiSplit(argvals = list(1:5), M = list(5,6), eFunType = list("Fourier"), eValType = "linear"),
               "simMultiSplit: argvals, M, eFunType, eValType must all be of length 1!")
  expect_error(funData:::simMultiWeight(argvals = list(1:5, 1:6, 1:7), M = list(5,6,7), eFunType = list("Fourier"), eValType = "linear"),
               "Function simMultiWeight: method is not implemented for objects of dimension > 2!")
  expect_error(funData:::simMultiWeight(argvals = list(list(1:25), list(1:5,1:4)), M = list(8,c(4,5)), eFunType = list("Fourier"), eValType = "linear"),
               "Function simMultiWeight: basis dimensions must be equal!")

  # check split version
set.seed(1)
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
})

test_that("sparsify",{
  # univariate functional data
  set.seed(1)
  f <- simFunData(argvals = seq(0,1, 0.01), M = 10, eFunType = "Fourier", eValType = "linear", N = 2)$simData
  
  # check errors:
  expect_error(sparsify(f, minObs = -1, maxObs = 5), "Sparsification: 'minObs' must be a positive integer!")
  expect_error(sparsify(f, minObs = 1, maxObs = nObsPoints(f)+1), "Sparsification: 'maxObs' must not exceed the maximal number of observations")
  expect_error(sparsify(f, minObs = 5, maxObs = 2), "Sparsification: 'minObs' must be smaller or equal to 'maxObs'.")
  
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

