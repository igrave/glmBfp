test_that("coxTBF works with uc terms", {
  set.seed(102024)
  dat <- survival::veteran
  dat$status <- dat$status == 1
  suppressWarnings(
    object <- coxTBF(
      Surv(time, status) ~ uc(trt) + uc(age) + uc(karno) + uc(celltype) + uc(diagtime) + uc(prior),
      data = dat,
    )
  )

  result <- predict.TBFcox(object, newdata = dat[1:5, ], times = c(100, 200))
  expect_equal(
    result,
    matrix(
      c(
        0.583350689483257, 0.669476472133622, 0.583350689483257,
        0.583350689483257, 0.669476472133622, 0.321489366340356, 0.42962238594794,
        0.321489366340356, 0.321489366340356, 0.42962238594794
      ),
      nrow = 5,
      ncol = 2,
      dimnames = list(NULL, c("100", "200"))
    )
  )
})

test_that("predict works with coxTBF with sep = TRUE", {
  set.seed(102024)
  dat <- survival::veteran
  dat$status <- dat$status == 1
  suppressWarnings(
    object <- coxTBF(
      Surv(time, status) ~ bfp(age, 1) + bfp(karno, 1) +
        bfp(diagtime, 1) + bfp(prior, 1),
      data = dat,
      sep = TRUE
    )
  )

  result <- predict.TBFcox.sep(object, newdata = dat[1:5, ], times = c(100, 200))
  expect_equal(
    result,
    matrix(
      c(
        0.404734875321373, 0.52178924629397, 0.404734875321373,
        0.404734875321373, 0.52178924629397, 0.174250039340219, 0.285054600725943,
        0.174250039340219, 0.174250039340219, 0.285054600725943
      ),
      nrow = 5,
      ncol = 2,
      dimnames = list(NULL, c("100", "200"))
    )
  )
})



test_that("predict.TBFcox.BMA works", {
  set.seed(102024)
  dat <- survival::veteran
  dat$status <- dat$status == 1
  suppressWarnings(
    object <- coxTBF(
      Surv(time, status) ~ uc(trt) + uc(age) + uc(karno) + uc(celltype) + uc(diagtime) + uc(prior),
      data = dat,
      IC = "BIC",
      type = "BMAfull"
    )
  )

  result <- predict.TBFcox.BMA(object, newdata = dat[1:5, ], times = c(100, 200))
  expect_equal(
    result,
    matrix(
      c(
        0.4051163842681, 0.527023583809112, 0.4051163842681,
        0.4051163842681, 0.527023583809112, 0.173655771907254, 0.289100000810177,
        0.173655771907254, 0.173655771907254, 0.289100000810177
      ),
      nrow = 5,
      ncol = 2
    )
  )
})
