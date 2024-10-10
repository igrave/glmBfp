test_that("coxTBF works with uc terms", {
  set.seed(102024)
  dat <- survival::veteran
  dat$status <- dat$status == 1
  suppressWarnings(
    result <- coxTBF(
      Surv(time, status) ~ uc(trt) + uc(age) + uc(karno) + uc(celltype) + uc(diagtime) + uc(prior),
      data = dat,
    )
  )

  expect_equal(
    result$coefs,
    c(
      celltypesmallcell = 0.682195527580969, celltypeadeno = 1.10579342810208,
      celltypelarge = 0.314212090584748, karno = -0.0295046132201781
    )
  )
})

test_that("coxTBF works with bfp terms", {
  set.seed(102024)
  dat <- survival::veteran
  dat$status <- dat$status == 1
  suppressWarnings(
    result <- coxTBF(
      Surv(time, status) ~ bfp(age, 1) + bfp(karno, 1) +
        bfp(diagtime, 1) + bfp(prior, 1),
      data = dat,
    )
  )

  expect_equal(result$coefs, c("karno^1" = -3.2772937))
})

test_that("coxTBF works with globalEB", {
  set.seed(102024)
  dat <- survival::veteran
  dat$status <- dat$status == 1
  suppressWarnings(
    result <- coxTBF(
      Surv(time, status) ~ uc(trt) + uc(age) + uc(karno) + uc(celltype) + uc(diagtime) + uc(prior),
      data = dat,
      globalEB = TRUE
    )
  )

  expect_equal(
    result$coefs,
    c(
      celltypesmallcell = 0.649233020541543, celltypeadeno = 1.04989976693073,
      celltypelarge = 0.297255116709026, karno = -0.0281576412870105
    )
  )
})

test_that("coxTBF works with BIC", {
  set.seed(102024)
  dat <- survival::veteran
  dat$status <- dat$status == 1
  suppressWarnings(
    result <- coxTBF(
      Surv(time, status) ~ uc(trt) + uc(age) + uc(karno) + uc(celltype) + uc(diagtime) + uc(prior),
      data = dat,
      IC = "BIC"
    )
  )

  expect_equal(
    result$coefs,
    c(karno = -0.0333985449212696)
  )
})


test_that("coxTBF works with type BMA", {
  set.seed(102024)
  dat <- survival::veteran
  dat$status <- dat$status == 1
  suppressWarnings(
    result <- coxTBF(
      Surv(time, status) ~ uc(trt) + uc(age) + uc(karno) + uc(celltype) + uc(diagtime) + uc(prior),
      data = dat,
      type = "BMA",
      keepModelList = TRUE,
      nModels = 100
    )
  )

  expect_equal(
    result$coefs,
    c(
      age = -0.00202419478319447, celltypesmallcell = 0.668781469573777,
      celltypeadeno = 1.01530341497844, celltypelarge = 0.306546076175885,
      diagtime = 0.000448314201362383, karno = -0.0296825412852211,
      prior = 0.00192812445037288, trt = 0.0979056973859562
    )
  )
})

test_that("coxTBF works with type MPM", {
  set.seed(102024)
  dat <- survival::veteran
  dat$status <- dat$status == 1
  suppressWarnings(
    result <- coxTBF(
      Surv(time, status) ~ uc(trt) + uc(age) + uc(karno) + uc(celltype) + uc(diagtime) + uc(prior),
      data = dat,
      type = "MPM",
      keepModelList = TRUE,
      nModels = 100
    )
  )

  expect_equal(
    result$coefs,
    c(
      celltypesmallcell = 0.682195527580969,
      celltypeadeno = 1.10579342810208,
      celltypelarge = 0.314212090584748,
      karno = -0.0295046132201782
    )
  )
})


test_that("coxTBF works with type MAP", {
  set.seed(102024)
  dat <- survival::veteran
  dat$status <- dat$status == 1
  suppressWarnings(
    result <- coxTBF(
      Surv(time, status) ~ uc(trt) + uc(age) + uc(karno) + uc(celltype) + uc(diagtime) + uc(prior),
      data = dat,
      type = "MAP",
      keepModelList = TRUE,
      nModels = 100
    )
  )

  expect_equal(
    result$coefs,
    c(
      celltypesmallcell = 0.682195527580969,
      celltypeadeno = 1.10579342810208,
      celltypelarge = 0.314212090584748,
      karno = -0.0295046132201782
    )
  )
})
