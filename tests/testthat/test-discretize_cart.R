library(testthat)
library(dplyr)
library(rpart)

set.seed(8497)
sim_tr_cls <- sim_data_2class(1000)
sim_te_cls <- sim_data_2class(100)

set.seed(8497)
sim_tr_reg <- sim_data_reg(1000)
sim_te_reg <- sim_data_reg(100)

mod <- rpart(y ~ x, data = sim_tr_reg)

best_split <- unname(mod$splits[, "index"])

test_that("low-level binning for classification", {
  expect_no_error(
    splits <-
      embed:::cart_binning(
        sim_tr_cls$x,
        "x",
        sim_tr_cls$class,
        cost_complexity = 0.01,
        tree_depth = 5,
        min_n = 10
      )
  )
  expect_equal(splits, best_split)

  set.seed(283834)
  expect_snapshot({
    splits <-
      embed:::cart_binning(
        sample(sim_tr_cls$x),
        "x",
        sim_tr_cls$class,
        cost_complexity = 0.01,
        tree_depth = 5,
        min_n = 10
      )
  })
  expect_equal(splits, numeric(0))
})

test_that("low-level binning for regression", {
  expect_no_error(
    splits <-
      embed:::cart_binning(
        sim_tr_reg$x,
        "x",
        sim_tr_reg$y,
        cost_complexity = 0.01,
        tree_depth = 5,
        min_n = 10
      )
  )
  expect_equal(splits, best_split)

  set.seed(283834)
  expect_snapshot({
    splits <-
      embed:::cart_binning(
        sample(sim_tr_reg$x),
        "potato",
        sim_tr_reg$y,
        cost_complexity = 0.01,
        tree_depth = 5,
        min_n = 10
      )
  })
  expect_equal(splits, numeric(0))
})

test_that("step function for classification", {
  expect_snapshot({
    cart_rec <-
      recipe(class ~ ., data = sim_tr_cls) |>
      step_discretize_cart(all_predictors(), outcome = "class") |>
      prep()
  })

  expect_equal(names(cart_rec$steps[[1]]$rules), "x")
  expect_equal(cart_rec$steps[[1]]$rules$x, best_split)

  expect_no_error(
    cart_pred <- bake(cart_rec, sim_tr_cls[, -3])
  )

  expect_true(is.factor(cart_pred$x))
  expect_equal(length(levels(cart_pred$x)), 3)
  expect_true(is.numeric(cart_pred$z))
})

test_that("step function for regression", {
  expect_snapshot({
    cart_rec <-
      recipe(y ~ ., data = sim_tr_reg) |>
      step_discretize_cart(all_predictors(), outcome = "y") |>
      prep()
  })

  expect_equal(names(cart_rec$steps[[1]]$rules), "x")
  expect_equal(cart_rec$steps[[1]]$rules$x, best_split)

  expect_no_error(
    cart_pred <- bake(cart_rec, sim_tr_reg[, -3])
  )

  expect_true(is.factor(cart_pred$x))
  expect_equal(length(levels(cart_pred$x)), 3)
  expect_true(is.numeric(cart_pred$z))
})

test_that("bad args", {
  tmp <- sim_tr_reg
  tmp$w <- sample(letters[1:4], nrow(tmp), replace = TRUE)

  expect_snapshot(error = TRUE, {
    cart_rec <-
      recipe(y ~ ., data = tmp) |>
      step_discretize_cart(all_predictors(), outcome = "y") |>
      prep()
  })
})

test_that("tidy method", {
  cart_rec <-
    recipe(y ~ ., data = sim_tr_reg) |>
    step_discretize_cart(all_predictors(), outcome = "y")

  res <- tidy(cart_rec, number = 1)
  expect_equal(
    res$terms,
    "all_predictors()"
  )
  expect_equal(
    res$value,
    NA_real_
  )

  expect_snapshot({
    cart_rec <- prep(cart_rec)
  })

  res <- tidy(cart_rec, number = 1)
  expect_equal(
    res$terms,
    rep("x", 2)
  )
  expect_equal(
    res$value,
    best_split
  )
})

test_that("case weights step functions", {
  sim_tr_cls_cw <- sim_tr_cls |>
    mutate(weight = importance_weights(rep(0:1, each = 500)))

  sim_tr_reg_cw <- sim_tr_reg |>
    mutate(weight = importance_weights(rep(0:1, each = 500)))

  mod_cw <- rpart(y ~ x, data = sim_tr_reg, weights = rep(0:1, each = 500))
  best_split_cw <- unname(mod_cw$splits[, "index"])

  # Classification
  expect_snapshot({
    cart_rec <-
      recipe(class ~ ., data = sim_tr_cls_cw) |>
      step_discretize_cart(all_predictors(), outcome = "class") |>
      prep()
  })

  expect_equal(names(cart_rec$steps[[1]]$rules), "x")
  expect_equal(cart_rec$steps[[1]]$rules$x, best_split_cw)

  # Regression
  expect_snapshot({
    cart_rec <-
      recipe(y ~ ., data = sim_tr_reg_cw) |>
      step_discretize_cart(all_predictors(), outcome = "y") |>
      prep()
  })

  expect_equal(names(cart_rec$steps[[1]]$rules), c("x", "z"))
  expect_equal(cart_rec$steps[[1]]$rules$x, best_split_cw)

  expect_snapshot(cart_rec)
})

test_that("tunable", {
  rec <-
    recipe(~., data = mtcars) |>
    step_discretize_cart(all_predictors(), outcome = "mpg")
  rec_param <- tunable.step_discretize_cart(rec$steps[[1]])
  expect_equal(rec_param$name, c("cost_complexity", "tree_depth", "min_n"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 3)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_discretize_cart(outcome = vars("mpg"), cost_complexity = -4) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_discretize_cart(outcome = vars("mpg"), min_n = -4) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_discretize_cart(outcome = vars("mpg"), tree_depth = -4) |>
      prep()
  )
})


# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(class ~ ., data = sim_tr_cls) |>
    step_discretize_cart(x, z, outcome = "class") |>
    update_role(x, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  expect_snapshot(
    rec_trained <- prep(rec, training = sim_tr_cls, verbose = FALSE)
  )

  expect_snapshot(
    error = TRUE,
    bake(rec_trained, new_data = sim_tr_cls[, -1])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_discretize_cart(rec, outcome = "mpg")

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_discretize_cart(rec1, outcome = "mpg")

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_discretize_cart(rec, outcome = "mpg")

  expect <- tibble(terms = character(), value = double(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(class ~ ., data = sim_tr_cls) |>
    step_discretize_cart(all_predictors(), outcome = "class")

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to works with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_discretize_cart(
      all_predictors(),
      outcome = "mpg",
      cost_complexity = hardhat::tune(),
      tree_depth = hardhat::tune(),
      min_n = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 3L)
})
