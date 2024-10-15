test_that("testCox works", {
  library(survival)
  n <- 100
  set.seed(123)
  x <- matrix(rnorm(2 * n), n, 2)
  beta <- c(0.5, 0.1)
  eta <- x %*% beta
  time <- rexp(n, exp(eta))
  status <- rbinom(n, 1, 0.5)

  result_no_cpp <- testCox(
    survTimes = time,
    censInd = status == 1,
    X = x,
    useCppCode = FALSE
  )

  result_cpp <- testCox(
    survTimes = time,
    censInd = status == 1,
    X = x,
    useCppCode = TRUE
  )

  expect_equal(setNames(result_no_cpp$betas, NULL), result_cpp$betas[, 1])
  expect_equal(result_no_cpp$cov, result_cpp$cov)
  expect_equal(result_no_cpp$deviance, result_cpp$deviance)
})
