library(testthat)
library(dplyr)
library(rpart)

source(test_path("make_binned_data.R"))

set.seed(8497)
sim_tr_cls <- sim_data_2class(1000)
sim_te_cls <- sim_data_2class(100)

set.seed(8497)
sim_tr_reg <- sim_data_reg(1000)
sim_te_reg <- sim_data_reg(100)

mod <- rpart(y ~ x, data = sim_tr_reg)

best_split <- unname(mod$splits[, "index"])

test_that("low-level binning for classification", {
  expect_error(
    splits <-
      embed:::cart_binning(
        sim_tr_cls$x,
        "x",
        sim_tr_cls$class,
        cost_complexity = 0.01,
        tree_depth = 5,
        min_n = 10
      ),
    regexp = NA
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
  expect_error(
    splits <-
      embed:::cart_binning(
        sim_tr_reg$x,
        "x",
        sim_tr_reg$y,
        cost_complexity = 0.01,
        tree_depth = 5,
        min_n = 10
      ),
    regexp = NA
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
      recipe(class ~ ., data = sim_tr_cls) %>%
      step_discretize_cart(all_predictors(), outcome = "class") %>%
      prep()
  })

  expect_equal(names(cart_rec$steps[[1]]$rules), "x")
  expect_equal(cart_rec$steps[[1]]$rules$x, best_split)

  expect_error(
    cart_pred <- bake(cart_rec, sim_tr_cls[, -3]),
    regexp = NA
  )

  expect_true(is.factor(cart_pred$x))
  expect_equal(length(levels(cart_pred$x)), 3)
  expect_true(is.numeric(cart_pred$z))
})

test_that("step function for regression", {
  expect_snapshot({
    cart_rec <-
      recipe(y ~ ., data = sim_tr_reg) %>%
      step_discretize_cart(all_predictors(), outcome = "y") %>%
      prep()
  })

  expect_equal(names(cart_rec$steps[[1]]$rules), "x")
  expect_equal(cart_rec$steps[[1]]$rules$x, best_split)

  expect_error(
    cart_pred <- bake(cart_rec, sim_tr_reg[, -3]),
    regexp = NA
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
      recipe(y ~ ., data = tmp) %>%
      step_discretize_cart(all_predictors(), outcome = "y") %>%
      prep()
  })
})

test_that("tidy method", {
  cart_rec <-
    recipe(y ~ ., data = sim_tr_reg) %>%
    step_discretize_cart(all_predictors(), outcome = "y")

  res <- tidy(cart_rec, number = 1)
  expect_equal(
    res$variable,
    "all_predictors()"
  )
  expect_equal(
    res$values,
    NA_character_
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
    res$values,
    best_split
  )
})

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(class ~ ., data = sim_tr_cls) %>%
    step_discretize_cart(x, z, outcome = "class") %>%
    update_role(x, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  expect_warning(
    rec_trained <- prep(rec, training = sim_tr_cls, verbose = FALSE)
  )

  expect_error(
    bake(rec_trained, new_data = sim_tr_cls[, -1]),
    class = "new_data_missing_column"
  )
})

test_that("printing", {
  cart_rec <-
    recipe(class ~ ., data = sim_tr_cls) %>%
    step_discretize_cart(all_predictors(), outcome = "class")

  expect_snapshot(cart_rec)
  expect_snapshot(prep(cart_rec))
})

test_that("empty selections", {
  data(ad_data, package = "modeldata")
  expect_error(
    rec <-
      recipe(Class ~ Genotype + tau, data = ad_data) %>%
      step_discretize_cart(starts_with("potato"), outcome = "Class") %>%
      prep(),
    regexp = NA
  )
  expect_equal(
    bake(rec, new_data = NULL),
    ad_data %>% select(Genotype, tau, Class)
  )
})

test_that("case weights step functions", {
  sim_tr_cls_cw <- sim_tr_cls %>%
    mutate(weight = importance_weights(rep(0:1, each = 500)))

  sim_tr_reg_cw <- sim_tr_reg %>%
    mutate(weight = importance_weights(rep(0:1, each = 500)))

  mod_cw <- rpart(y ~ x, data = sim_tr_reg, weights = rep(0:1, each = 500))
  best_split_cw <- unname(mod_cw$splits[, "index"])

  # Classification
  expect_snapshot({
    cart_rec <-
      recipe(class ~ ., data = sim_tr_cls_cw) %>%
      step_discretize_cart(all_predictors(), outcome = "class") %>%
      prep()
  })

  expect_equal(names(cart_rec$steps[[1]]$rules), "x")
  expect_equal(cart_rec$steps[[1]]$rules$x, best_split_cw)

  # Regression
  expect_snapshot({
    cart_rec <-
      recipe(y ~ ., data = sim_tr_reg_cw) %>%
      step_discretize_cart(all_predictors(), outcome = "y") %>%
      prep()
  })

  expect_equal(names(cart_rec$steps[[1]]$rules), c("x", "z"))
  expect_equal(cart_rec$steps[[1]]$rules$x, best_split_cw)

  expect_snapshot(cart_rec)
})