test_that("sampleGlm works with default arguments", {
  library(MASS)
  pima <- rbind(Pima.tr, Pima.te)
  pima$hasDiabetes <- as.numeric(pima$type == "Yes")
  pima.nObs <- nrow(pima)
  ## define the prior distributions for g which we are going to use:
  expect_warning(
    prior <- InvGammaGPrior(a=1 / 2, b=pima.nObs / 2),
    "normalized"
  )
  
  set.seed(102)
  object <- glmBayesMfp(type ~ 
                               bfp(npreg) + 
                               bfp(glu) + 
                               bfp(bp) +
                               bfp(skin) + 
                               bfp(bmi) + 
                               bfp(ped) + 
                               bfp(age),
                             data = pima,
                             family = binomial("logit"),
                             priorSpecs = list(gPrior = prior, modelPrior = "sparse"),
                             nModels = 10L,
                             chainlength = 40L,
                             method = "sampling",
                             useOpenMP = FALSE,
                             higherOrderCorrection = TRUE)
  
  result <- sampleGlm(object, mcmc = McmcOptions(iterations = 1000L, burnin = 500L))
  
  expect_equal(result$logMargLik$estimate, c("numeratorTerms" = -250.316506))
  expect_equal(
    rowMeans(result$coefficients),
    c(`(Intercept)` = -0.974450810919336,
      `age^-0.5` = -6.46500176298788, 
      `bmi^-0.5` = -3.64775122259271,
      `glu^1` = 3.41388134240106,
      `log(npreg)` = 0.176193476530033
    )
  )
  
  expect_equal(
    result$samples@z[1:4],
    c(5.09647434137287, 4.68772977233472, 4.69960506172403, 4.89929624952311)
  )
  
})
