source(testthat::test_path("test-helpers.R"))

data(cells, package = "modeldata")
cells$case <- cells$class <- NULL
cells <- as.data.frame(scale(cells))

split <- seq.int(1, 2019, by = 10)
tr <- cells[-split, ]
te <- cells[split, ]

test_that("step_pca_truncated", {
  skip_if_not_installed("irlba")
  
  rec <-
    recipe(~., data = tr) %>%
    step_pca_truncated(
      all_predictors(),
      num_comp = 4
    ) %>%
    prep()
  
  direct_mod <- irlba::prcomp_irlba(as.matrix(tr), n = 4)
  direct_coef <- direct_mod$rotation
  embed_coef <- rec$steps[[1]]$res$rotation
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
})

test_that("check_name() is used", {
  skip_if_not_installed("irlba")
  
  dat <- tr
  dat$PC1 <- dat$var_inten_ch_1
  
  rec <- rec <-
    recipe(~., data = dat) %>%
    step_pca_truncated(all_predictors())
  
  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(tr) %>%
    step_pca_truncated(
      avg_inten_ch_1, avg_inten_ch_2, avg_inten_ch_3, avg_inten_ch_4,
      num_comp = 1
    ) %>%
    update_role(avg_inten_ch_1, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)
  
  rec_trained <- prep(rec, training = tr, verbose = FALSE)
  
  expect_error(
    bake(rec_trained, new_data = tr[, -3]),
    class = "new_data_missing_column"
  )
})

test_that("printing", {
  rec <- recipe(mpg ~ ., data = mtcars) %>%
    step_pca_truncated(all_predictors(), num_comp = 2)
  
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})