test_that("step_pca_truncated", {
  skip_if_not_installed("irlba")
  skip_if_not_installed("modeldata")

  data(cells, package = "modeldata")
  cells$case <- cells$class <- NULL
  cells <- as.data.frame(scale(cells))

  split <- seq.int(1, 2019, by = 10)
  tr <- cells[-split, ]
  te <- cells[split, ]

  rec <-
    recipe(~., data = tr) |>
    step_pca_truncated(
      all_predictors(),
      num_comp = 4
    ) |>
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
  skip_if_not_installed("modeldata")

  data(cells, package = "modeldata")
  cells$case <- cells$class <- NULL
  cells <- as.data.frame(scale(cells))

  split <- seq.int(1, 2019, by = 10)
  tr <- cells[-split, ]
  te <- cells[split, ]

  dat <- tr
  dat$PC1 <- dat$var_inten_ch_1

  rec <- rec <-
    recipe(~., data = dat) |>
    step_pca_truncated(all_predictors())

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("Do nothing for num_comps = 0 and keep_original_cols = FALSE", {
  # https://github.com/tidymodels/recipes/issues/1152

  rec <- recipe(carb ~ ., data = mtcars) |>
    step_pca_truncated(
      all_predictors(),
      num_comp = 0,
      keep_original_cols = FALSE
    ) |>
    prep()

  res <- bake(rec, new_data = NULL)

  expect_identical(res, tibble::as_tibble(mtcars))
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_pca_truncated(num_comp = -4) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_pca_truncated(prefix = NULL)
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("modeldata")

  data(cells, package = "modeldata")
  cells$case <- cells$class <- NULL
  cells <- as.data.frame(scale(cells))

  split <- seq.int(1, 2019, by = 10)
  tr <- cells[-split, ]
  te <- cells[split, ]

  rec <- recipe(tr) |>
    step_pca_truncated(
      avg_inten_ch_1,
      avg_inten_ch_2,
      avg_inten_ch_3,
      avg_inten_ch_4,
      num_comp = 1
    ) |>
    update_role(avg_inten_ch_1, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = tr, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(rec_trained, new_data = tr[, -3])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_pca_truncated(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_pca_truncated(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_pca_truncated(rec)

  expect <- tibble(
    terms = character(),
    value = double(),
    component = character(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  skip_if_not_installed("irlba")
  skip_if_not_installed("modeldata")

  data(cells, package = "modeldata")
  cells$case <- cells$class <- NULL
  cells <- as.data.frame(scale(cells))

  new_names <- c("PC1")

  rec <- recipe(~., data = cells) |>
    step_pca_truncated(
      all_predictors(),
      num_comp = 1,
      keep_original_cols = FALSE
    )

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~., data = cells) |>
    step_pca_truncated(
      all_predictors(),
      num_comp = 1,
      keep_original_cols = TRUE
    )

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c(names(cells), new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  skip_if_not_installed("irlba")
  skip_if_not_installed("modeldata")

  data(cells, package = "modeldata")
  cells$case <- cells$class <- NULL
  cells <- as.data.frame(scale(cells))

  rec <- recipe(~., data = cells) |>
    step_pca_truncated(all_predictors(), num_comp = 1)

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = cells)
  )
})

test_that("printing", {
  rec <- recipe(mpg ~ ., data = mtcars) |>
    step_pca_truncated(all_predictors(), num_comp = 2)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
