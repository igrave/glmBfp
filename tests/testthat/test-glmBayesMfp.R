test_that("glmBayesMfp works with bfp", {
  library(MASS)
  pima <- rbind(Pima.tr, Pima.te)
  pima$hasDiabetes <- as.numeric(pima$type == "Yes")
  pima.nObs <- nrow(pima)
  ## define the prior distributions for g which we are going to use:
  expect_warning(
    prior <- InvGammaGPrior(a = 1 / 2, b = pima.nObs / 2),
    "normalized"
  )

  set.seed(102)
  models.pima <- glmBayesMfp(
    type ~ bfp(npreg) + bfp(glu) + bfp(bp) + bfp(skin) +
      bfp(bmi) + bfp(ped) + bfp(age),
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
    c(
      0.350331748045138, 0.257695154941687, 0.208844659491879, 0.101454601042585,
      0.0460499879084113, 0.0266479161490211, 0.00671566423380901,
      0.00161366858999095, 0.000189431194827148, 0.000185917150804001
    )
  )
})

test_that("glmBayesMfp works with test bayes factors", {
  library(MASS)
  pima <- rbind(Pima.tr, Pima.te)
  pima$hasDiabetes <- as.numeric(pima$type == "Yes")
  pima.nObs <- nrow(pima)
  ## define the prior distributions for g which we are going to use:
  expect_warning(
    prior <- InvGammaGPrior(a = 1 / 2, b = pima.nObs / 2),
    "normalized"
  )

  set.seed(102)
  models.pima <- glmBayesMfp(
    type ~ bfp(npreg) + bfp(glu) + bfp(bp) +
      bfp(skin) + bfp(bmi) + bfp(ped) + bfp(age),
    data = pima,
    family = binomial("logit"),
    priorSpecs = list(gPrior = prior, modelPrior = "sparse"),
    nModels = 10L,
    chainlength = 40L,
    method = "sampling",
    useOpenMP = FALSE,
    higherOrderCorrection = TRUE,
    tbf = TRUE
  )

  expect_equal(attr(models.pima, "numVisited"), 34)
  expect_equal(length(models.pima), 10L)
  models_df <- as.data.frame(models.pima)
  expect_equal(
    posteriors(models.pima),
    c(`1` = 0.360157607178329, `2` = 0.243679639668586, `3` = 0.211431377255278, 
      `4` = 0.103169056731385, `5` = 0.041885765304427, `6` = 0.0308511807437347, 
      `7` = 0.00614873263236583, `8` = 0.00190136742250369, `9` = 0.0002145736187449, 
      `10` = 0.00020714592227717)
  )
  
  expect_equal(
    models_df[1, ],
    data.frame(
      posterior = 0.360157607178329,
      logMargLik = 88.7500024022889, 
      logPrior = -16.0080521873961, 
      age = "-0.5",
      bmi = "-0.5", 
      bp = "",
      glu = "1",
      npreg = "0",
      ped = "",
      skin = "",
      row.names = "1"
    )
  )
})

test_that("glmBayesMfp works with fixed covariates", {
  library(MASS)
  pima <- rbind(Pima.tr, Pima.te)
  pima$hasDiabetes <- as.numeric(pima$type == "Yes")
  pima.nObs <- nrow(pima)
  ## define the prior distributions for g which we are going to use:
  expect_warning(
    prior <- InvGammaGPrior(a = 1 / 2, b = pima.nObs / 2),
    "normalized"
  )
  
  set.seed(102)
  models.pima <- glmBayesMfp(
    type ~ bfp(npreg, max = 1) + uc(glu) +
      uc(bmi) + ped + age,
    data = pima,
    family = binomial("logit"),
    priorSpecs = list(gPrior = prior, modelPrior = "sparse"),
    nModels = 40L,
    chainlength = 40L,
    method = "exhaustive",
    useOpenMP = FALSE,
    higherOrderCorrection = TRUE
  )
  
  expect_equal(attr(models.pima, "numVisited"), 37)
  expect_equal(length(models.pima), 37L)
  models_df <- as.data.frame(models.pima)
  expect_equal(models_df$age, c(rep(TRUE, 36), FALSE))
  expect_equal(models_df$ped, c(rep(TRUE, 36), FALSE))
  expect_equal(
    inclusionProbs(models.pima),
    c(npreg = 0.70834218, bmi = 0.99987822, glu = 1)
  )
})
