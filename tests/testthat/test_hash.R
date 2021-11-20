source(testthat::test_path("make_example_data.R"))
source(testthat::test_path("test_helpers.R"))

# ------------------------------------------------------------------------------

test_that("basic usage", {
  skip_on_cran()
  
  rec <- recipe(x1 ~ x3, data = ex_dat) %>% 
    step_feature_hash(x3)
  
  expect_error(rec_tr <- prep(rec), regex = NA)
  
  res_tr <- juice(rec_tr, dplyr::starts_with("x3"))
    
  expect_equal(ncol(res_tr), 2^6)
  expect_true(all(grepl("^x3_hash_", names(res_tr)))) 
  
  check_1 <-
    keras::text_hashing_trick(as.character(ex_dat$x3[1]),
                              2 ^ 6,
                              filters = "",
                              lower = FALSE)
  for(j in 1:ncol(res_tr)) {
    if (j == check_1) {
      expect_true(res_tr[1, j][[1]] == 1)
    } else {
      expect_true(res_tr[1, j][[1]] == 0)
    }
  }
  
  check_2<-
    keras::text_hashing_trick("bridge 4",
                              2 ^ 6,
                              filters = "",
                              split = "please dont",
                              lower = FALSE)
  te_dat <- tibble(x1 = 1.1, x3 = "bridge 4")
  
  expect_warning(
    res_te <- bake(rec_tr, te_dat, dplyr::starts_with("x3"))
  )
  
  for(j in 1:ncol(res_te)) {
    if (j == check_2) {
      expect_true(res_te[1, j][[1]] == 1)
    } else {
      expect_true(res_te[1, j][[1]] == 0)
    }
  }
  
})


test_that("basic usage - character strings", {
  skip_on_cran()
  
  ex_dat$x3 <- as.character(ex_dat$x3)
  
  rec <- recipe(x1 ~ x3, data = ex_dat) %>% 
    step_feature_hash(x3)
  
  expect_error(rec_tr <- prep(rec), regex = NA)
  
  res_tr <- juice(rec_tr, dplyr::starts_with("x3"))
  
  expect_equal(ncol(res_tr), 2^6)
  expect_true(all(grepl("^x3_hash_", names(res_tr)))) 
  
  check_1 <-
    keras::text_hashing_trick(as.character(ex_dat$x3[1]),
                              2 ^ 6,
                              filters = "",
                              lower = FALSE)
  for(j in 1:ncol(res_tr)) {
    if (j == check_1) {
      expect_true(res_tr[1, j][[1]] == 1)
    } else {
      expect_true(res_tr[1, j][[1]] == 0)
    }
  }
  
  check_2<-
    keras::text_hashing_trick("bridge 4",
                              2 ^ 6,
                              filters = "",
                              split = "please dont",
                              lower = FALSE)
  te_dat <- tibble(x1 = 1.1, x3 = "bridge 4")
  
  res_te <- bake(rec_tr, te_dat, dplyr::starts_with("x3"))
  
  for(j in 1:ncol(res_te)) {
    if (j == check_2) {
      expect_true(res_te[1, j][[1]] == 1)
    } else {
      expect_true(res_te[1, j][[1]] == 0)
    }
  }
  
})

test_that('keep_original_cols works', {
  skip_on_cran()
  
  rec <- recipe(x1 ~ x3, data = ex_dat) %>% 
    step_feature_hash(x3, num_hash = 9, keep_original_cols = TRUE)
  
  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
  hash_pred <- bake(rec_trained, new_data = new_dat, all_predictors())
  
  expect_equal(
    colnames(hash_pred),
    c("x3", paste0("x3_hash_", 1:9))
  )
})

test_that('can prep recipes with no keep_original_cols', {
  skip_on_cran()
  
  rec <- recipe(x1 ~ x3, data = ex_dat) %>% 
    step_feature_hash(x3, num_hash = 9, keep_original_cols = TRUE)
  
  rec$steps[[1]]$keep_original_cols <- NULL

  expect_warning(
    rec_trained <- prep(rec, training = ex_dat, verbose = FALSE),
    "'keep_original_cols' was added to"
  )
  
  expect_error(
    hash_pred <- bake(rec_trained, new_data = new_dat, all_predictors()),
    NA
  )
  
})
