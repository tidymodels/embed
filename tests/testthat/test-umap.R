iris_dat <- iris

iris_dat[, 1:4] <- scale(iris_dat[, 1:4])

split <- seq.int(1, 150, by = 9)
tr <- iris_dat[-split, ]
te <- iris_dat[split, ]

test_that("factor outcome", {
  skip_if_not_installed("irlba", "2.3.5.2")

  set.seed(11)
  supervised <-
    recipe(Species ~ ., data = tr) |>
    step_umap(all_predictors(), outcome = vars(Species), num_comp = 2) |>
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
  skip_if_not_installed("irlba", "2.3.5.2")

  set.seed(11)
  supervised <-
    recipe(Sepal.Length ~ ., data = tr[, -5]) |>
    step_umap(all_predictors(), outcome = vars(Sepal.Length), num_comp = 2) |>
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
    bake(
      supervised,
      new_data = te[, -5],
      composition = "matrix",
      all_predictors()
    ),
    ignore_attr = TRUE
  )
})

test_that("metric argument works", {
  skip_if_not_installed("irlba", "2.3.5.2")

  set.seed(11)
  unsupervised <-
    recipe(~., data = tr[, -5]) |>
    step_umap(
      all_predictors(),
      num_comp = 3,
      min_dist = .2,
      learn_rate = .2,
      metric = "hamming"
    ) |>
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
    bake(
      unsupervised,
      new_data = te[, -5],
      composition = "matrix",
      all_predictors()
    ),
    ignore_attr = TRUE
  )
})

test_that("no outcome", {
  skip_if_not_installed("irlba", "2.3.5.2")

  set.seed(11)
  unsupervised <-
    recipe(~., data = tr[, -5]) |>
    step_umap(
      all_predictors(),
      num_comp = 3,
      min_dist = .2,
      learn_rate = .2
    ) |>
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
    bake(
      unsupervised,
      new_data = te[, -5],
      composition = "matrix",
      all_predictors()
    ),
    ignore_attr = TRUE
  )
})

test_that("check_name() is used", {
  skip_if_not_installed("irlba", "2.3.5.2")

  dat <- tr
  dat$UMAP1 <- dat$Species

  rec <- recipe(Species ~ ., data = dat) |>
    step_umap(all_predictors(), num_comp = 2)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = mtcars) |>
    step_umap(all_predictors())
  rec_param <- tunable.step_umap(rec$steps[[1]])
  expect_equal(
    rec_param$name,
    c(
      "num_comp",
      "neighbors",
      "min_dist",
      "learn_rate",
      "epochs",
      "initial",
      "target_weight"
    )
  )
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 7L)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("backwards compatible for initial and target_weight args (#213)", {
  skip_if_not_installed("irlba", "2.3.5.2")

  rec <- recipe(Species ~ ., data = tr) |>
    step_umap(all_predictors(), num_comp = 2)

  exp_res <- prep(rec)

  rec$steps[[1]]$initial <- NULL
  rec$steps[[1]]$target_weight <- NULL

  expect_identical(
    prep(rec),
    exp_res
  )
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_umap(num_comp = -4) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_umap(neighbors = -4) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_umap(min_dist = TRUE) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_umap(learn_rate = -4) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_umap(epochs = -4) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_umap(initial = "wrong") |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_umap(target_weight = -4) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_umap(prefix = NULL)
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("irlba", "2.3.5.2")

  rec <- recipe(Species ~ ., data = tr) |>
    step_umap(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) |>
    update_role(Petal.Width, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = tr, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(rec_trained, new_data = tr[, -4])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_umap(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_umap(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_umap(rec)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  skip_if_not_installed("irlba", "2.3.5.2")

  new_names <- c("UMAP1", "UMAP2", "UMAP3")

  rec <- recipe(~., data = tr[, -5]) |>
    step_umap(
      all_predictors(),
      num_comp = 3,
      min_dist = .2,
      learn_rate = .2,
      keep_original_cols = FALSE
    )

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(~., data = tr[, -5]) |>
    step_umap(
      all_predictors(),
      num_comp = 3,
      min_dist = .2,
      learn_rate = .2,
      keep_original_cols = TRUE
    )

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  skip_if_not_installed("irlba", "2.3.5.2")

  rec <- recipe(~mpg, mtcars) |>
    step_umap(all_predictors())

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = mtcars)
  )
})

test_that("printing", {
  skip_if_not_installed("irlba", "2.3.5.2")

  rec <- recipe(~., data = tr[, -5]) |>
    step_umap(all_predictors())

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to works with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_umap(
      all_predictors(),
      num_comp = hardhat::tune(),
      neighbors = hardhat::tune(),
      min_dist = hardhat::tune(),
      learn_rate = hardhat::tune(),
      epochs = hardhat::tune(),
      initial = hardhat::tune(),
      target_weight = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 7L)
})
