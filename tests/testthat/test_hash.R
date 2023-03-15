source(testthat::test_path("make_example_data.R"))
source(testthat::test_path("test_helpers.R"))

# ------------------------------------------------------------------------------

test_that("basic usage", {
  skip_on_cran()
  skip_if_not_installed("keras")
  skip_if(is.null(tensorflow::tf_version()))
  rlang::local_options(lifecycle_verbosity = "quiet")

  rec <- recipe(x1 ~ x3, data = ex_dat) %>%
    step_feature_hash(x3)

  expect_error(rec_tr <- prep(rec), regex = NA)

  res_tr <- bake(rec_tr, new_data = NULL, dplyr::starts_with("x3"))

  expect_equal(ncol(res_tr), 2^6)
  expect_true(all(grepl("^x3_hash_", names(res_tr))))

  check_1 <-
    keras::text_hashing_trick(as.character(ex_dat$x3[1]),
      2^6,
      filters = "",
      lower = FALSE
    )
  for (j in seq_len(ncol(res_tr))) {
    if (j == check_1) {
      expect_true(res_tr[1, j][[1]] == 1)
    } else {
      expect_true(res_tr[1, j][[1]] == 0)
    }
  }

  check_2 <-
    keras::text_hashing_trick("bridge 4",
      2^6,
      filters = "",
      split = "please dont",
      lower = FALSE
    )
  te_dat <- tibble(x1 = 1.1, x3 = "bridge 4")

  expect_snapshot(
    res_te <- bake(rec_tr, te_dat, dplyr::starts_with("x3"))
  )

  for (j in seq_len(ncol(res_te))) {
    if (j == check_2) {
      expect_true(res_te[1, j][[1]] == 1)
    } else {
      expect_true(res_te[1, j][[1]] == 0)
    }
  }
})

test_that("basic usage - character strings", {
  skip_on_cran()
  skip_if_not_installed("keras")
  skip_if(is.null(tensorflow::tf_version()))
  rlang::local_options(lifecycle_verbosity = "quiet")

  ex_dat$x3 <- as.character(ex_dat$x3)

  rec <- recipe(x1 ~ x3, data = ex_dat) %>%
    step_feature_hash(x3)

  expect_error(rec_tr <- prep(rec), regex = NA)

  res_tr <- bake(rec_tr, new_data = NULL, dplyr::starts_with("x3"))

  expect_equal(ncol(res_tr), 2^6)
  expect_true(all(grepl("^x3_hash_", names(res_tr))))

  check_1 <-
    keras::text_hashing_trick(as.character(ex_dat$x3[1]),
      2^6,
      filters = "",
      lower = FALSE
    )
  for (j in seq_len(ncol(res_tr))) {
    if (j == check_1) {
      expect_true(res_tr[1, j][[1]] == 1)
    } else {
      expect_true(res_tr[1, j][[1]] == 0)
    }
  }

  check_2 <-
    keras::text_hashing_trick("bridge 4",
      2^6,
      filters = "",
      split = "please dont",
      lower = FALSE
    )
  te_dat <- tibble(x1 = 1.1, x3 = "bridge 4")

  res_te <- bake(rec_tr, te_dat, dplyr::starts_with("x3"))

  for (j in seq_len(ncol(res_te))) {
    if (j == check_2) {
      expect_true(res_te[1, j][[1]] == 1)
    } else {
      expect_true(res_te[1, j][[1]] == 0)
    }
  }
})

test_that("keep_original_cols works", {
  skip_on_cran()
  skip_if_not_installed("keras")
  skip_if(is.null(tensorflow::tf_version()))
  rlang::local_options(lifecycle_verbosity = "quiet")

  rec <- recipe(x1 ~ x3, data = ex_dat) %>%
    step_feature_hash(x3, num_hash = 9, keep_original_cols = TRUE)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  hash_pred <- bake(rec_trained, new_data = new_dat, all_predictors())

  expect_equal(
    colnames(hash_pred),
    c("x3", paste0("x3_hash_", 1:9))
  )
})

test_that("can prep recipes with no keep_original_cols", {
  skip_on_cran()
  skip_if_not_installed("keras")
  skip_if(is.null(tensorflow::tf_version()))
  rlang::local_options(lifecycle_verbosity = "quiet")

  rec <- recipe(x1 ~ x3, data = ex_dat) %>%
    step_feature_hash(x3, num_hash = 9, keep_original_cols = TRUE)

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  )

  expect_error(
    hash_pred <- bake(rec_trained, new_data = new_dat, all_predictors()),
    NA
  )
})

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_on_cran()
  skip_if_not_installed("keras")
  skip_if(is.null(tensorflow::tf_version()))
  rlang::local_options(lifecycle_verbosity = "quiet")
  rec <- recipe(x2 ~ ., data = ex_dat) %>%
    step_feature_hash(x3) %>%
    update_role(x3, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)

  expect_error(
    bake(rec_trained, new_data = ex_dat[, -3]),
    class = "new_data_missing_column"
  )
})

test_that("printing", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  print_test <- recipe(x1 ~ x3, data = ex_dat) %>%
    step_feature_hash(x3)
  expect_snapshot(print_test)

  skip_if_not_installed("keras")
  skip_if(is.null(tensorflow::tf_version()))
  expect_snapshot(prep(print_test))
})

# ------------------------------------------------------------------------------

test_that("empty selections", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  data(ad_data, package = "modeldata")
  expect_error(
    rec <-
      recipe(Class ~ Genotype + tau, data = ad_data) %>%
      step_feature_hash(starts_with("potato")) %>%
      prep(),
    regexp = NA
  )
  expect_equal(
    bake(rec, new_data = NULL),
    ad_data %>% select(Genotype, tau, Class)
  )
})
