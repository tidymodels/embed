source(testthat::test_path("test-helpers.R"))

iris_dat <- iris

iris_dat[, 1:4] <- scale(iris_dat[, 1:4])

split <- seq.int(1, 150, by = 9)
tr <- iris_dat[-split, ]
te <- iris_dat[split, ]

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

  expect_equal(
    direct_mod$embedding,
    supervised$steps[[1]]$object$embedding,
    ignore_attr = TRUE
  )

  # predictions:

  direct_pred <-
    withr::with_seed(
      supervised$steps[[1]]$seed[2],
      uwot::umap_transform(model = direct_mod, X = te[, 1:4])
    )
  colnames(direct_pred) <- paste0("umap_", 1:2)
  expect_equal(
    direct_pred,
    bake(supervised, new_data = te, composition = "matrix", all_predictors()),
    ignore_attr = TRUE
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

  expect_equal(
    direct_mod$embedding,
    supervised$steps[[1]]$object$embedding,
    ignore_attr = TRUE
  )

  # predictions:

  direct_pred <-
    withr::with_seed(
      supervised$steps[[1]]$seed[2],
      uwot::umap_transform(model = direct_mod, X = te[, 2:4])
    )
  colnames(direct_pred) <- paste0("umap_", 1:2)
  expect_equal(
    direct_pred,
    bake(supervised, new_data = te[, -5], composition = "matrix", all_predictors()),
    ignore_attr = TRUE
  )
})

test_that("metric argument works", {
  set.seed(11)
  unsupervised <-
    recipe(~., data = tr[, -5]) %>%
    step_umap(
      all_predictors(),
      num_comp = 3, min_dist = .2, learn_rate = .2, metric = "hamming"
    ) %>%
    prep(training = tr[, -5])
  
  direct_mod <-
    withr::with_seed(
      unsupervised$steps[[1]]$seed[1],
      uwot::umap(
        X = tr[, -5],
        n_neighbors = 15,
        n_components = 3,
        metric = "hamming",
        learning_rate = .2,
        min_dist = 0.2,
        verbose = FALSE,
        n_threads = 1,
        ret_model = TRUE
      )
    )
  
  expect_equal(
    direct_mod$embedding,
    unsupervised$steps[[1]]$object$embedding,
    ignore_attr = TRUE
  )
  
  # predictions:
  
  direct_pred <-
    withr::with_seed(
      unsupervised$steps[[1]]$seed[2],
      uwot::umap_transform(model = direct_mod, X = te[, -5])
    )
  colnames(direct_pred) <- paste0("umap_", 1:3)
  expect_equal(
    direct_pred,
    bake(unsupervised, new_data = te[, -5], composition = "matrix", all_predictors()),
    ignore_attr = TRUE
  )
})

test_that("no outcome", {
  set.seed(11)
  unsupervised <-
    recipe(~., data = tr[, -5]) %>%
    step_umap(all_predictors(), num_comp = 3, min_dist = .2, learn_rate = .2) %>%
    prep(training = tr[, -5])

  direct_mod <-
    withr::with_seed(
      unsupervised$steps[[1]]$seed[1],
      uwot::umap(
        X = tr[, -5],
        n_neighbors = 15,
        n_components = 3,
        learning_rate = .2,
        min_dist = 0.2,
        verbose = FALSE,
        n_threads = 1,
        ret_model = TRUE
      )
    )

  expect_equal(
    direct_mod$embedding,
    unsupervised$steps[[1]]$object$embedding,
    ignore_attr = TRUE
  )

  # predictions:

  direct_pred <-
    withr::with_seed(
      unsupervised$steps[[1]]$seed[2],
      uwot::umap_transform(model = direct_mod, X = te[, -5])
    )
  colnames(direct_pred) <- paste0("umap_", 1:3)
  expect_equal(
    direct_pred,
    bake(unsupervised, new_data = te[, -5], composition = "matrix", all_predictors()),
    ignore_attr = TRUE
  )
})

test_that("keep_original_cols works", {
  set.seed(11)
  unsupervised <-
    recipe(~., data = tr[, -5]) %>%
    step_umap(all_predictors(),
      num_comp = 3, min_dist = .2, learn_rate = .2,
      keep_original_cols = TRUE
    ) %>%
    prep(training = tr[, -5])

  umap_pred <- bake(unsupervised, new_data = te[, -5], composition = "matrix", all_predictors())

  expect_equal(
    colnames(umap_pred),
    c(
      "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
      "UMAP1", "UMAP2", "UMAP3"
    )
  )
})

test_that("can prep recipes with no keep_original_cols", {
  set.seed(11)
  unsupervised <-
    recipe(~., data = tr[, -5]) %>%
    step_umap(all_predictors(), num_comp = 3, min_dist = .2, learn_rate = .2)

  unsupervised$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    umap_pred <- prep(unsupervised, training = tr[, -5], verbose = FALSE)
  )

  expect_error(
    umap_pred <- bake(umap_pred, new_data = te[, -5], all_predictors()),
    NA
  )
})

test_that("check_name() is used", {
  dat <- tr
  dat$UMAP1 <- dat$Species
  
  rec <- recipe(Species ~ ., data = dat) %>%
    step_umap(all_predictors(), num_comp = 2)
  
  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("printing", {
  print_test <- recipe(~., data = tr[, -5]) %>%
    step_umap(all_predictors())
  expect_snapshot(print_test)
  expect_snapshot(prep(print_test))
})

test_that("empty selections", {
  data(ad_data, package = "modeldata")
  expect_error(
    rec <-
      recipe(Class ~ Genotype + tau, data = ad_data) %>%
      step_umap(starts_with("potato"), outcome = vars(Class)) %>%
      prep(),
    regexp = NA
  )
  expect_equal(
    bake(rec, new_data = NULL),
    ad_data %>% select(Genotype, tau, Class)
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = mtcars) %>%
    step_umap(all_predictors())
  rec_param <- tunable.step_umap(rec$steps[[1]])
  expect_equal(
    rec_param$name,
    c("num_comp", "neighbors", "min_dist", "learn_rate", "epochs")
  )
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 5)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("tunable is setup to works with extract_parameter_set_dials works", {
  rec <- recipe(~., data = mtcars) %>%
    step_umap(
      all_predictors(),
      num_comp = hardhat::tune(),
      neighbors = hardhat::tune(),
      min_dist = hardhat::tune(),
      learn_rate = hardhat::tune(),
      epochs = hardhat::tune()
    )
  
  params <- extract_parameter_set_dials(rec)
  
  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 5L)
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(Species ~ ., data = tr) %>%
    step_umap(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
    update_role(Petal.Width, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)
  
  rec_trained <- prep(rec, training = tr, verbose = FALSE)
  
  expect_error(
    bake(rec_trained, new_data = tr[, -4]),
    class = "new_data_missing_column"
  )
})