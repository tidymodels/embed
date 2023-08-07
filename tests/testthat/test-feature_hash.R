source(testthat::test_path("make_example_data.R"))
source(testthat::test_path("test-helpers.R"))

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

test_that("check_name() is used", {
  skip_on_cran()
  skip_if_not_installed("keras")
  skip_if(is.null(tensorflow::tf_version()))
  rlang::local_options(lifecycle_verbosity = "quiet")
  
  dat <- ex_dat
  dat$x3_hash_01 <- dat$x3
  
  rec <- recipe(~., data = dat) %>%
    step_feature_hash(x3)
  
  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

# Infrastructure ---------------------------------------------------------------

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

test_that("empty printing", {
  skip_on_cran()
  skip_if_not_installed("keras")
  skip_if(is.null(tensorflow::tf_version()))
  rlang::local_options(lifecycle_verbosity = "quiet")
  
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_feature_hash(rec)
  
  expect_snapshot(rec)
  
  rec <- prep(rec, mtcars)
  
  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  skip_on_cran()
  skip_if_not_installed("keras")
  skip_if(is.null(tensorflow::tf_version()))
  rlang::local_options(lifecycle_verbosity = "quiet")
  
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_feature_hash(rec1)
  
  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)
  
  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)
  
  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  skip_on_cran()
  skip_if_not_installed("keras")
  skip_if(is.null(tensorflow::tf_version()))
  rlang::local_options(lifecycle_verbosity = "quiet")
  
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_feature_hash(rec)
  
  expect <- tibble(terms = character(), id = character())
  
  expect_identical(tidy(rec, number = 1), expect)
  
  rec <- prep(rec, mtcars)
  
  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  skip_on_cran()
  skip_if_not_installed("keras")
  skip_if(is.null(tensorflow::tf_version()))
  rlang::local_options(lifecycle_verbosity = "quiet")
  
  new_names <- paste0("x3_hash_", 1:9)
  
  rec <- recipe(~ x3, data = ex_dat) %>%
    step_feature_hash(x3, num_hash = 9, keep_original_cols = FALSE)
  
  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)
  
  expect_equal(
    colnames(res),
    new_names
  )
  
  rec <- recipe(~ x3, data = ex_dat) %>%
    step_feature_hash(x3, num_hash = 9, keep_original_cols = TRUE)
  
  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)
  
  expect_equal(
    colnames(res),
    c("x3", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  skip_on_cran()
  skip_if_not_installed("keras")
  skip_if(is.null(tensorflow::tf_version()))
  rlang::local_options(lifecycle_verbosity = "quiet")
  
  rec <- recipe(~ x3, data = ex_dat) %>%
    step_feature_hash(x3)
  
  rec$steps[[1]]$keep_original_cols <- NULL
  
  expect_snapshot(
    rec <- prep(rec)
  )
  
  expect_error(
    bake(rec, new_data = ex_dat),
    NA
  )
})

test_that("printing", {
  skip_if_not_installed("keras")
  skip_if(is.null(tensorflow::tf_version()))
  rlang::local_options(lifecycle_verbosity = "quiet")
  
  rec <- recipe(x1 ~ x3, data = ex_dat) %>%
    step_feature_hash(x3)
  
  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})