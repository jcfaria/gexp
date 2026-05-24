# test-gexp-core.R — smoke and regression tests for the gexp package.

test_that("simple CRD returns expected class and components", {
  crd <- gexp(
    mu  = 15,
    err = matrix(0, nrow = 6, ncol = 1),
    r   = 3,
    fe  = list(alpha = c(1, -2)),
    type = "SIMPLE",
    design = "CRD"
  )

  expect_s3_class(crd, "gexp.simple_crd")
  expect_s3_class(crd, "gexp")
  expect_true(all(c("X", "Y", "dfm") %in% names(crd)))
  expect_equal(nrow(crd$dfm), 6L)
  expect_equal(ncol(crd$Y), 1L)
})

test_that("factorial CRD runs with deterministic errors", {
  fe_crd <- gexp(
    mu  = 15,
    err = matrix(0, nrow = 8, ncol = 1),
    r   = 2,
    fl  = list(f1 = c("A", "B"), f2 = c("P", "Q")),
    fe  = list(f1 = c(1, -1), f2 = c(2, -2)),
    type = "FE",
    design = "CRD"
  )

  expect_s3_class(fe_crd, "gexp.fe_crd")
  expect_equal(nrow(fe_crd$dfm), 8L)
})

test_that("RCBD simple design returns expected dimensions", {
  rcbd <- gexp(
    mu  = 10,
    err = matrix(0, nrow = 12, ncol = 1),
    r   = 2,
    fe  = list(alpha = c(1, -1)),
    type = "SIMPLE",
    design = "RCBD"
  )

  expect_s3_class(rcbd, "gexp.simple_rcbd")
  expect_equal(nrow(rcbd$dfm), 12L)
})

test_that("summary and print methods run without error", {
  crd <- gexp(
    mu  = 15,
    err = matrix(0, nrow = 6, ncol = 1),
    r   = 3,
    fe  = list(alpha = c(1, -2)),
    type = "SIMPLE",
    design = "CRD"
  )

  expect_output(summary(crd), "Database")
  expect_output(print(crd), "Design Matrix")
})

test_that("simple LSD accepts rowl/coll as bare vectors or named lists", {
  lsd_vec <- gexp(
    mu     = 10,
    err    = matrix(0, nrow = 25, ncol = 1),
    r      = 1,
    fe     = list(trt = c(0, 1, 2, 3, 4)),
    rowe   = c(0, 1, 2, 3, 4),
    rowl   = paste0("r", 1:5),
    cole   = c(0, 1, 2, 3, 4),
    coll   = paste0("l", 1:5),
    type   = "SIMPLE",
    design = "LSD"
  )

  lsd_list <- gexp(
    mu     = 10,
    err    = matrix(0, nrow = 25, ncol = 1),
    r      = 1,
    fe     = list(trt = c(0, 1, 2, 3, 4)),
    rowe   = c(0, 1, 2, 3, 4),
    rowl   = list(Row = paste0("r", 1:5)),
    cole   = c(0, 1, 2, 3, 4),
    coll   = list(Column = paste0("l", 1:5)),
    type   = "SIMPLE",
    design = "LSD"
  )

  expect_s3_class(lsd_vec, "gexp.simple_lsd")
  expect_s3_class(lsd_list, "gexp.simple_lsd")
  expect_equal(nrow(lsd_vec$dfm), 25L)
  expect_equal(nrow(lsd_list$dfm), 25L)
  expect_equal(levels(lsd_vec$dfm$Row), paste0("r", 1:5))
  expect_equal(levels(lsd_list$dfm$Row), paste0("r", 1:5))
})

test_that("plot method runs without error", {
  crd <- gexp(
    mu  = 15,
    err = matrix(0, nrow = 6, ncol = 1),
    r   = 3,
    fe  = list(alpha = c(1, -2)),
    type = "SIMPLE",
    design = "CRD"
  )

  expect_no_error({
    pdf(NULL)
    on.exit(dev.off(), add = TRUE)
    plot(crd)
  })
})
