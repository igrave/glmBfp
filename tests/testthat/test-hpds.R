test_that("empiricalHpd works", {
  set.seed(900)
  result <- empiricalHpd(rnorm(1000000), level = 0.9)
  expect_equal(result, c(lower = -1.64971526, upper = 1.64275266))
})


test_that("scrHpd works", {
  set.seed(900)
  samples <- matrix(rnorm(1000000), ncol = 50000)
  result <- scrHpd(samples, level = 0.9)

  expect_matrix(result, nrows = 20, ncols = 2)
  expect_equal(result[1, ], c(lower = -2.7739137, upper = 2.8015672))
})
