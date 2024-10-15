test_that("evalZdensity works", {
  library(MASS)
  pima <- rbind(Pima.tr, Pima.te)
  pima$hasDiabetes <- as.numeric(pima$type == "Yes")
  pima.nObs <- nrow(pima)
  expect_warning(
    prior <- InvGammaGPrior(a = 1 / 2, b = pima.nObs / 2),
    "normalized"
  )
  
  set.seed(102)
  object <- glmBayesMfp(
    type ~ bfp(npreg) + bfp(glu) + bfp(bp) +
      bfp(skin) + bfp(bmi) + bfp(ped) + bfp(age),
    data = pima,
    family = binomial("logit"),
    priorSpecs = list(gPrior = prior, modelPrior = "sparse"),
    nModels = 10L,
    chainlength = 40L,
    method = "sampling",
    useOpenMP = FALSE,
    higherOrderCorrection = TRUE
  )

  z <- object[[1]]$information$negLogUnnormZDensities$args
  
  result <- evalZdensity(
    config = object[[1]]$configuration,
    object = object,
    zValues =  z,
    conditional = FALSE,
    debug = FALSE,
    higherOrderCorrection = TRUE
  )
  
  expect_equal(
    result,
    object[[1]]$information$negLogUnnormZDensities$vals
  )
})
