test_that("optimization routines work correctly", {
  ## define simple univariate functions
  funs <- list(
    f1 = function(x) log((x - 3)^2 + 0.1),
    f2 = function(x) (x - 3)^2,
    f3 = function(x) (x - 2)^4,
    f4 = function(x) abs(x - 3)
  )

  ## and constraints
  constraints <- c(min = -20, max = 20)

  ## and plot range
  xgrid <- seq(from = -30, to = 30, length = 201L)

  ## start value
  start <- 0

  par(mfrow = n2mfrow(length(funs)))

  results_list <- list()
  ## now process all functions
  for (fname in names(funs)) {
    f <- funs[[fname]]
    # plot(xgrid, f(xgrid), type="l")

    cat(
      "-------------------",
      "Now processing",
      paste(deparse(f), collapse = ""),
      "-------------------\n"
    )

    ## we can also compare with classic R code
    result0 <- optim(
      start,
      f,
      method = "L-BFGS-B",
      hessian = TRUE,
      lower = constraints["min"],
      upper = constraints["max"]
    )

    ## and then with C++ scalar bfgs code
    result1 <- glmBfp:::cppBfgs(
      start,
      f,
      verbose = TRUE,
      min.x = constraints["min"],
      max.x = constraints["max"]
    )

    ## now the new optimize code
    result2 <- glmBfp:::cppOptimize(
      f,
      min.x = constraints["min"],
      max.x = constraints["max"]
    )

    ## assemble overview matrix
    results <- data.frame(
      par = c(result0$par, result1$par, result2$par),
      inv.hessian = c(1 / result0$hessian, result1$inv.hessian, result2$inv.hessian),
      nEvaluations = c(NA, length(result1$evaluations$args), length(result2$evaluations$args))
    )
    rownames(results) <- c("optim", "cppBfgs", "cppOptimize")

    ## and print it
    results_list[[fname]] <- results
  }

  expect_equal(
    results_list$f1$par,
    c(3, 3.00000430, 3.00000130)
  )

  expect_equal(
    results_list$f2$par,
    c(3, 3.000000, 3.000000)
  )

  expect_equal(
    results_list$f3$par,
    c(1.9955433, 1.9865853, 1.9999971)
  )

  expect_equal(
    results_list$f4$par,
    c(3, 0, 2.9999842)
  )
})
