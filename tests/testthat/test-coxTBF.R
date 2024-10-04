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
