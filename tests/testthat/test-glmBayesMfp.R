test_that("glmBayesMfp works with bfp", {
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
  models.pima <- glmBayesMfp(type ~ 
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
  
  expect_equal(attr(models.pima, "numVisited"), 33)
  expect_equal(length(models.pima), 10L)
  models_df <- as.data.frame(models.pima)
  expect_equal(
    models_df$posterior,
    c(0.350331748045138, 0.257695154941687, 0.208844659491879, 0.101454601042585, 
      0.0460499879084113, 0.0266479161490211, 0.00671566423380901, 
      0.00161366858999095, 0.000189431194827148, 0.000185917150804001
    )
  )
})

test_that("glmBayesMfp works with marginal likelihood", {
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
  models.pima <- glmBayesMfp(type ~ 
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
                             higherOrderCorrection = TRUE
                             ) 
  
  expect_equal(attr(models.pima, "numVisited"), 33)
  expect_equal(length(models.pima), 10L)
  models_df <- as.data.frame(models.pima)
  expect_equal(
    models_df$posterior,
    c(0.350331748045138, 0.257695154941687, 0.208844659491879, 0.101454601042585, 
      0.0460499879084113, 0.0266479161490211, 0.00671566423380901, 
      0.00161366858999095, 0.000189431194827148, 0.000185917150804001
    )
  )
})
