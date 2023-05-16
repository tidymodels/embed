source(testthat::test_path("test-helpers.R"))

data(cells, package = "modeldata")
cells$case <- cells$class <- NULL
cells <- as.data.frame(scale(cells))

split <- seq.int(1, 2019, by = 10)
tr <- cells[-split, ]
te <- cells[split, ]

test_that("step_pca_sparse", {
  skip_if_not_installed("irlba")

  rec <-
    recipe(~., data = tr) %>%
    step_pca_sparse(
      all_predictors(),
      num_comp = 4,
      predictor_prop = 1 / 2
    ) %>%
    prep()

  direct_mod <- irlba::ssvd(as.matrix(tr), k = 4, n = ncol(tr) / 2)
  direct_coef <- direct_mod$v
  embed_coef <- rec$steps[[1]]$res
  vars <- rownames(embed_coef)
  dimnames(embed_coef) <- NULL
  dimnames(direct_coef) <- NULL

  expect_equal(abs(direct_coef), abs(embed_coef), tolerance = 0.1)

  tidy_coef <- tidy(rec, number = 1)
  # test a few values
  expect_equal(
    tidy_coef$value[
      tidy_coef$terms == "angle_ch_1" & tidy_coef$component == "PC1"
    ],
    embed_coef[which(vars == "angle_ch_1"), 1]
  )

  expect_equal(
    tidy_coef$value[
      tidy_coef$terms == "total_inten_ch_3" & tidy_coef$component == "PC3"
    ],
    embed_coef[which(vars == "total_inten_ch_3"), 3]
  )

  expect_snapshot(rec)
})

test_that("check_name() is used", {
  skip_if_not_installed("irlba")
  
  dat <- tr
  dat$PC1 <- dat$var_inten_ch_1
  
  rec <- rec <-
    recipe(~., data = dat) %>%
    step_pca_sparse(
      all_predictors(),
      num_comp = 4,
      predictor_prop = 1 / 2
    )
  
  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = mtcars) %>%
    step_pca_sparse(all_predictors())
  rec_param <- tunable.step_pca_sparse(rec$steps[[1]])
  expect_equal(rec_param$name, c("num_comp", "predictor_prop"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("tunable is setup to works with extract_parameter_set_dials works", {
  rec <- recipe(~., data = mtcars) %>%
    step_pca_sparse(
      all_predictors(),
      num_comp = hardhat::tune(),
      predictor_prop = hardhat::tune()
    )
  
  params <- extract_parameter_set_dials(rec)
  
  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 2L)
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(tr) %>%
    step_pca_sparse(
      avg_inten_ch_1, avg_inten_ch_2, avg_inten_ch_3, avg_inten_ch_4,
      num_comp = 1,
      predictor_prop = 1 / 2
    ) %>%
    update_role(avg_inten_ch_1, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)
  
  rec_trained <- prep(rec, training = tr, verbose = FALSE)
  
  expect_error(
    bake(rec_trained, new_data = tr[, -3]),
    class = "new_data_missing_column"
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_pca_sparse(rec)
  
  expect_snapshot(rec)
  
  rec <- prep(rec, mtcars)
  
  expect_snapshot(rec)
})