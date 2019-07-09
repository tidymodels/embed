library(embed)
library(dplyr)
library(testthat)

# ------------------------------------------------------------------------------

iris_dat <- iris

iris_dat[, 1:4] <- scale(iris_dat[, 1:4])

split <- seq.int(1, 150, by = 9)
tr <- iris_dat[-split, ]
te <- iris_dat[ split, ]

# ------------------------------------------------------------------------------



context("umap")

test_that("factor outcome", {
  
  set.seed(11)
  supervised <- 
    recipe(Species ~ ., data = tr) %>%
    step_umap(all_predictors(), outcome = vars(Species), num_comp = 2) %>% 
    prep(training = tr)
  
  direct_mod <-
    withr::with_seed(
      supervised$steps[[1]]$seed[1],
      uwot::umap(
        X = tr[, 1:4],
        y = tr$Species,
        n_neighbors = 15,
        n_components = 2,
        learning_rate = 1,
        min_dist = 0.01,
        verbose = FALSE,
        n_threads = 1,
        ret_model = TRUE
      )
    )
  
  expect_equal(direct_mod$embedding, supervised$steps[[1]]$object$embedding)
  
  # predictions:
  
  direct_pred <- 
    withr::with_seed(
      supervised$steps[[1]]$seed[2],
      uwot::umap_transform(model = direct_mod, X = te[, 1:4])
    )
  colnames(direct_pred) <- paste0("umap_", 1:2)
  expect_equal(
    direct_pred,
    bake(supervised, new_data = te, composition = "matrix", all_predictors())
  )
  
})



test_that("numeric outcome", {
  
  set.seed(11)
  supervised <- 
    recipe(Sepal.Length ~ ., data = tr[, -5]) %>%
    step_umap(all_predictors(), outcome = vars(Sepal.Length), num_comp = 2) %>% 
    prep(training = tr[, -5])
  
  direct_mod <-
    withr::with_seed(
      supervised$steps[[1]]$seed[1],
      uwot::umap(
        X = tr[, 2:4],
        y = tr$Sepal.Length,
        n_neighbors = 15,
        n_components = 2,
        learning_rate = 1,
        min_dist = 0.01,
        verbose = FALSE,
        n_threads = 1,
        ret_model = TRUE
      )
    )
  
  expect_equal(direct_mod$embedding, supervised$steps[[1]]$object$embedding)
  
  # predictions:
  
  direct_pred <- 
    withr::with_seed(
      supervised$steps[[1]]$seed[2],
      uwot::umap_transform(model = direct_mod, X = te[, 2:4])
    )
  colnames(direct_pred) <- paste0("umap_", 1:2)
  expect_equal(
    direct_pred,
    bake(supervised, new_data = te[, -5], composition = "matrix", all_predictors())
  )
  
})


test_that("no outcome", {
  
  set.seed(11)
  unsupervised <- 
    recipe( ~ ., data = tr[, -5]) %>%
    step_umap(all_predictors(), num_comp = 3, min_dist = .2, learn_rate = .2) %>% 
    prep(training = tr[, -5])
  
  direct_mod <-
    withr::with_seed(
      unsupervised$steps[[1]]$seed[1],
      uwot::umap(
        X = tr[,-5],
        n_neighbors = 15,
        n_components = 3,
        learning_rate = .2,
        min_dist = 0.2,
        verbose = FALSE,
        n_threads = 1,
        ret_model = TRUE
      )
    )
  
  expect_equal(direct_mod$embedding, unsupervised$steps[[1]]$object$embedding)
  
  # predictions:
  
  direct_pred <- 
    withr::with_seed(
      unsupervised$steps[[1]]$seed[2],
      uwot::umap_transform(model = direct_mod, X = te[, -5])
    )
  colnames(direct_pred) <- paste0("umap_", 1:3)
  expect_equal(
    direct_pred,
    bake(unsupervised, new_data = te[, -5], composition = "matrix", all_predictors())
  )
  
})


