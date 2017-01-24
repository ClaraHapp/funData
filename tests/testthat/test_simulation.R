context("Test simulation methods")

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

