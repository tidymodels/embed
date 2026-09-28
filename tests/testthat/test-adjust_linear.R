rlang::local_options(lifecycle_verbosity = "quiet")

test_that("step_adjust_linear adjusts (simple)", {
  dat <- tibble::tibble(
    y = 10:15,
    batch = c(0, 0, 1, 1, 2, 2)
  )

  rec <- recipe(y ~ ., data = dat) |>
    step_adjust_linear(y, remove_vars = vars(batch)) |>
    prep(training = dat)

  baked <- bake(rec, new_data = dat)
  expect_identical(names(baked), "y")
  expect_equal(unname(baked$y), rep(c(12, 13), times = 3), tolerance = 1e-6)
})

test_that("step_adjust_linear adjusts (complex)", {
  data(mtcars)
  mtcars$cyl <- factor(mtcars$cyl)

  rec <- recipe(~., data = mtcars) |>
    step_adjust_linear(
      mpg,
      remove_vars = vars(cyl, wt, hp),
      keep_vars = vars(am)
    ) |>
    prep(training = mtcars)

  baked <- bake(rec, new_data = mtcars)

  mtcars_centered <- mtcars
  mtcars_centered[, c("wt", "hp", "am")] <-
    scale(
      mtcars[, c("wt", "hp", "am")],
      scale = FALSE
    )

  mod1 <- lm(
    mpg ~ cyl + wt + hp + am,
    contrasts = list(cyl = "contr.sum"),
    data = mtcars_centered
  )

  expect_identical(
    coef(rec$steps[[1]]$models$mpg),
    coef(mod1),
    ignore_attr = TRUE
  )

  expect_identical(
    baked$mpg,
    mtcars$mpg -
      rowSums(predict(
        mod1,
        newdata = mtcars_centered,
        type = "terms"
      )[, 1:3]),

    ignore_attr = TRUE
  )
})

test_that("step_adjust_linear basic drop options", {
  dat <- tibble::tibble(
    y = c(10, 12, 14, 16, 18, 20),
    z = c(5, 6, 7, 8, 9, 10),
    batch = c(0, 0, 1, 1, 2, 2),
    group = factor(c("a", "a", "a", "b", "b", "b"))
  )

  rec_remove <- recipe(y ~ ., data = dat) |>
    step_adjust_linear(
      y,
      remove_vars = vars(batch),
      keep_vars = vars(group),
      drop = "remove"
    ) |>
    prep(training = dat)

  baked_remove <- bake(rec_remove, new_data = dat)
  expect_false("batch" %in% names(baked_remove))
  expect_true("group" %in% names(baked_remove))
  expect_false(isTRUE(all.equal(baked_remove$y, dat$y)))

  rec_both <- recipe(y ~ ., data = dat) |>
    step_adjust_linear(
      y,
      remove_vars = vars(batch),
      keep_vars = vars(group),
      drop = "both"
    ) |>
    prep(training = dat)

  baked_both <- bake(rec_both, new_data = dat)
  expect_false("batch" %in% names(baked_both))
  expect_false("group" %in% names(baked_both))

  rec_none <- recipe(y ~ ., data = dat) |>
    step_adjust_linear(
      y,
      remove_vars = vars(batch),
      keep_vars = vars(group),
      drop = "none"
    ) |>
    prep(training = dat)

  baked_none <- bake(rec_none, new_data = dat)
  expect_true(all(c("batch", "group") %in% names(baked_none)))
})

test_that("step_adjust_linear can adjust multiple outcomes", {
  dat <- tibble::tibble(
    y = c(10, 12, 14, 16, 18, 20),
    z = c(20, 21, 22, 23, 24, 25),
    batch = c(0, 0, 1, 1, 2, 2),
    group = factor(c("a", "a", "a", "b", "b", "b"))
  )

  rec <- recipe(~., data = dat) |>
    step_adjust_linear(
      y,
      z,
      remove_vars = vars(batch),
      keep_vars = vars(group),
      drop = "none"
    ) |>
    prep(training = dat)

  baked <- bake(rec, new_data = dat)
  expect_false(isTRUE(all.equal(baked$y, dat$y)))
  expect_false(isTRUE(all.equal(baked$z, dat$z)))
})

test_that("step_adjust_linear validates arguments", {
  dat <- tibble::tibble(
    y = c(10, 12, 14, 16),
    batch = c(0, 0, 1, 1),
    group = factor(c("a", "a", "b", "b")),
    bad = as.Date("2020-01-01") + 0:3
  )

  expect_error(
    recipe(y ~ ., data = dat) |>
      step_adjust_linear(y) |>
      prep(training = dat),
    "remove_vars"
  )

  expect_error(
    recipe(y ~ ., data = dat) |>
      step_adjust_linear(
        y,
        remove_vars = vars(batch, group),
        keep_vars = vars(group)
      ) |>
      prep(training = dat),
    "disjoint"
  )

  expect_error(
    recipe(y ~ ., data = dat) |>
      step_adjust_linear(y, remove_vars = vars(bad)) |>
      prep(training = dat),
    "either factors or numeric"
  )
})

test_that("step_adjust_linear tidy works before and after prep", {
  dat <- tibble::tibble(
    y = c(10, 12, 14, 16, 18, 20),
    batch = c(0, 0, 1, 1, 2, 2),
    group = factor(c("a", "a", "a", "b", "b", "b"))
  )

  rec_untrained <- recipe(y ~ ., data = dat) |>
    step_adjust_linear(
      y,
      remove_vars = vars(batch),
      keep_vars = vars(group),
      id = "adj"
    )

  td_untrained <- tidy(rec_untrained, number = 1)
  expect_true(all(
    c("variables", "term", "type", "value", "id") %in% names(td_untrained)
  ))
  expect_true(all(td_untrained$id == "adj"))

  rec_trained <- prep(rec_untrained, training = dat)
  td_trained <- tidy(rec_trained, number = 1)
  expect_true(nrow(td_trained) > 0)
  expect_true(all(c("remove", "keep") %in% unique(td_trained$type)))
})

test_that("step_adjust_linear bake errors when required columns are missing", {
  dat <- tibble::tibble(
    y = c(10, 12, 14, 16, 18, 20),
    batch = c(0, 0, 1, 1, 2, 2),
    group = factor(c("a", "a", "a", "b", "b", "b"))
  )

  rec <- recipe(y ~ ., data = dat) |>
    step_adjust_linear(
      y,
      remove_vars = vars(batch),
      keep_vars = vars(group),
      drop = "none"
    )

  rec_trained <- prep(rec, training = dat, verbose = FALSE)

  expect_error(
    bake(rec_trained, new_data = dplyr::select(dat, -batch)),
    "required"
  )
})

test_that("step_adjust_linear can use case weights", {
  skip_if_not_installed("hardhat")

  dat <- tibble::tibble(
    y = c(1, 2, 3, 6, 9, 30),
    batch = c(0, 0, 1, 1, 2, 2),
    wts = hardhat::importance_weights(c(1, 1, 1, 1, 1, 20))
  )

  rec_weighted <- recipe(y ~ ., data = dat) |>
    step_adjust_linear(y, remove_vars = vars(batch), drop = "none") |>
    prep(training = dat)

  rec_unweighted <- recipe(y ~ ., data = dplyr::select(dat, -wts)) |>
    step_adjust_linear(y, remove_vars = vars(batch), drop = "none") |>
    prep(training = dplyr::select(dat, -wts))

  baked_weighted <- bake(rec_weighted, new_data = dplyr::select(dat, -wts))
  baked_unweighted <- bake(rec_unweighted, new_data = dplyr::select(dat, -wts))

  expect_false(isTRUE(all.equal(baked_weighted$y, baked_unweighted$y)))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  dat <- tibble::tibble(
    y = c(10, 12, 14, 16, 18, 20),
    batch = c(0, 0, 1, 1, 2, 2),
    group = factor(c("a", "a", "a", "b", "b", "b"))
  )

  rec <- recipe(y ~ ., data = dat) |>
    step_adjust_linear(
      y,
      remove_vars = vars(batch),
      keep_vars = vars(group)
    ) |>
    update_role(batch, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = dat, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(rec_trained, new_data = dat[, -2])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_adjust_linear(rec, remove_vars = vars(cyl))

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_adjust_linear(rec1, remove_vars = vars(cyl), drop = "none")

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_adjust_linear(rec, remove_vars = vars(cyl))

  expect <- tibble::tibble(
    variables = character(),
    term = character(),
    type = character(),
    value = double(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  dat <- tibble::tibble(
    y = c(10, 12, 14, 16, 18, 20),
    batch = c(0, 0, 1, 1, 2, 2),
    group = factor(c("a", "a", "a", "b", "b", "b"))
  )

  rec <- recipe(y ~ ., data = dat) |>
    step_adjust_linear(y, remove_vars = vars(batch), keep_vars = vars(group))

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
