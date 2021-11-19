library(testthat)
library(dplyr)
library(rpart)

context("discretize_cart")

source(test_path("make_binned_data.R"))

# ------------------------------------------------------------------------------

set.seed(8497)
sim_tr_cls <- sim_data_2class(1000)
sim_te_cls <- sim_data_2class(100)

set.seed(8497)
sim_tr_reg <- sim_data_reg(1000)
sim_te_reg <- sim_data_reg(100)

mod <- rpart(y ~ x, data = sim_tr_reg)

best_split <- unname(mod$splits[, "index"])

# ------------------------------------------------------------------------------

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
 expect_warning(
   splits <-
     embed:::cart_binning(
       sample(sim_tr_cls$x),
       "x",
       sim_tr_cls$class,
       cost_complexity = 0.01,
       tree_depth = 5,
       min_n = 10
     ),
   regexp = "failed to find any meaningful splits for predictor 'x'"
 )
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
  expect_warning(
    splits <-
      embed:::cart_binning(
        sample(sim_tr_reg$x),
        "potato",
        sim_tr_reg$y,
        cost_complexity = 0.01,
        tree_depth = 5,
        min_n = 10
      ),
    regexp = "failed to find any meaningful splits for predictor 'potato'"
  )
  expect_equal(splits, numeric(0))
  
})

# ------------------------------------------------------------------------------

test_that("step function for classification", {
  
  expect_warning(
    cart_rec <-
      recipe(class ~ ., data = sim_tr_cls) %>%
      step_discretize_cart(all_predictors(), outcome = "class") %>% 
      prep(),
    regexp = "failed to find any meaningful splits for predictor 'z'"
  )
  
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
  
  expect_warning(
    cart_rec <-
      recipe(y ~ ., data = sim_tr_reg) %>%
      step_discretize_cart(all_predictors(), outcome = "y") %>% 
      prep(),
    regexp = "failed to find any meaningful splits for predictor 'z'"
  )
  
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

# ------------------------------------------------------------------------------

test_that("bad args", {
  tmp <- sim_tr_reg
  tmp$w <- sample(letters[1:4], nrow(tmp), replace = TRUE)
  
  expect_error(
    cart_rec <-
      recipe(y ~ ., data = tmp) %>%
      step_discretize_cart(all_predictors(), outcome = "y") %>% 
      prep(),
    regexp = "All columns selected for the step should be numeric"
  )
  
})

# ------------------------------------------------------------------------------

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
  
  expect_warning(
    cart_rec <- prep(cart_rec),
    "failed to find any meaningful splits for predictor 'z'"
  )
  
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


# ------------------------------------------------------------------------------


test_that("printing", {
  cart_rec <-
    recipe(class ~ ., data = sim_tr_cls) %>%
    step_discretize_cart(all_predictors(), outcome = "class")
  
  expect_output(print(cart_rec))
  expect_output(
    expect_warning(prep(cart_rec, verbose = TRUE),
                   "failed to find any meaningful splits for predictor 'z'")
  )
})
