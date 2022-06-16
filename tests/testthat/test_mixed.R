source(testthat::test_path("make_example_data.R"))
source(testthat::test_path("test_helpers.R"))

# ------------------------------------------------------------------------------

test_that("factor encoded predictor", {
  skip_if_not_installed("lme4")
  class_test <- recipe(x2 ~ ., data = ex_dat) %>%
    step_lencode_mixed(x3, outcome = vars(x2), id = "id") %>%
    prep(training = ex_dat, retain = TRUE)
  tr_values <- juice(class_test)$x3
  new_values <- bake(class_test, new_data = new_dat)
  expect_snapshot(
    new_values_ch <- bake(class_test, new_data = new_dat_ch)
  )
  key <- class_test$steps[[1]]$mapping
  td_obj <- tidy(class_test, number = 1)

  expect_equal("x3", names(key))

  expect_equal(
    length(unique(ex_dat$x3)) + 1,
    nrow(key$x3)
  )
  expect_true(sum(key$x3$..level == "..new") == 1)

  expect_true(is.numeric(tr_values))

  expect_equal(
    new_values$x3[1],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values$x3[2],
    key$x3$..value[key$x3$..level == levels(ex_dat$x3)[1]]
  )
  expect_equal(
    new_values$x3[3],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values_ch$x3[1],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values_ch$x3[2],
    key$x3$..value[key$x3$..level == levels(ex_dat$x3)[1]]
  )
  expect_equal(
    new_values_ch$x3[3],
    key$x3$..value[key$x3$..level == "..new"]
  )

  expect_equal(
    td_obj$level,
    key$x3$..level
  )
  expect_equal(
    td_obj$value,
    key$x3$..value
  )
})

test_that("character encoded predictor", {
  skip_if_not_installed("lme4")
  class_test <- recipe(x2 ~ ., data = ex_dat_ch) %>%
    step_lencode_mixed(x3, outcome = vars(x2)) %>%
    prep(training = ex_dat_ch, retain = TRUE)
  tr_values <- juice(class_test)$x3
  new_values <- bake(class_test, new_data = new_dat_ch)
  new_values_fc <- bake(class_test, new_data = new_dat)
  key <- class_test$steps[[1]]$mapping
  td_obj <- tidy(class_test, number = 1)

  expect_equal("x3", names(key))

  expect_equal(
    length(unique(ex_dat$x3)) + 1,
    nrow(key$x3)
  )
  expect_true(sum(key$x3$..level == "..new") == 1)

  expect_true(is.numeric(tr_values))

  expect_equal(
    new_values$x3[1],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values$x3[2],
    key$x3$..value[key$x3$..level == levels(ex_dat$x3)[1]]
  )
  expect_equal(
    new_values$x3[3],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values_fc$x3[1],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values_fc$x3[2],
    key$x3$..value[key$x3$..level == levels(ex_dat$x3)[1]]
  )
  expect_equal(
    new_values_fc$x3[3],
    key$x3$..value[key$x3$..level == "..new"]
  )

  expect_equal(
    td_obj$level,
    key$x3$..level
  )
  expect_equal(
    td_obj$value,
    key$x3$..value
  )
})

###################################################################

test_that("factor encoded predictor", {
  skip_if_not_installed("lme4")
  reg_test <- recipe(x1 ~ ., data = ex_dat) %>%
    step_lencode_mixed(x3, outcome = vars(x1)) %>%
    prep(training = ex_dat, retain = TRUE)
  tr_values <- juice(reg_test)$x3
  new_values <- bake(reg_test, new_data = new_dat)
  expect_snapshot(
    new_values_ch <- bake(reg_test, new_data = new_dat_ch)
  )
  td_obj <- tidy(reg_test, number = 1)

  key <- reg_test$steps[[1]]$mapping

  expect_equal("x3", names(key))

  expect_equal(
    length(unique(ex_dat$x3)) + 1,
    nrow(key$x3)
  )
  expect_true(sum(key$x3$..level == "..new") == 1)

  expect_true(is.numeric(tr_values))

  expect_equal(
    new_values$x3[1],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values$x3[2],
    key$x3$..value[key$x3$..level == levels(ex_dat$x3)[1]]
  )
  expect_equal(
    new_values$x3[3],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values_ch$x3[1],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values_ch$x3[2],
    key$x3$..value[key$x3$..level == levels(ex_dat$x3)[1]]
  )
  expect_equal(
    new_values_ch$x3[3],
    key$x3$..value[key$x3$..level == "..new"]
  )

  expect_equal(
    td_obj$level,
    key$x3$..level
  )
  expect_equal(
    td_obj$value,
    key$x3$..value
  )
})

test_that("character encoded predictor", {
  skip_if_not_installed("lme4")
  reg_test <- recipe(x1 ~ ., data = ex_dat_ch) %>%
    step_lencode_mixed(x3, outcome = vars(x1)) %>%
    prep(training = ex_dat_ch, retain = TRUE)
  tr_values <- juice(reg_test)$x3
  new_values <- bake(reg_test, new_data = new_dat_ch)
  new_values_fc <- bake(reg_test, new_data = new_dat)
  key <- reg_test$steps[[1]]$mapping
  td_obj <- tidy(reg_test, number = 1)

  expect_equal("x3", names(key))

  expect_equal(
    length(unique(ex_dat$x3)) + 1,
    nrow(key$x3)
  )
  expect_true(sum(key$x3$..level == "..new") == 1)

  expect_true(is.numeric(tr_values))

  expect_equal(
    new_values$x3[1],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values$x3[2],
    key$x3$..value[key$x3$..level == levels(ex_dat$x3)[1]]
  )
  expect_equal(
    new_values$x3[3],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values_fc$x3[1],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values_fc$x3[2],
    key$x3$..value[key$x3$..level == levels(ex_dat$x3)[1]]
  )
  expect_equal(
    new_values_fc$x3[3],
    key$x3$..value[key$x3$..level == "..new"]
  )

  expect_equal(
    td_obj$level,
    key$x3$..level
  )
  expect_equal(
    td_obj$value,
    key$x3$..value
  )
})


###################################################################

test_that("bad args", {
  skip_if_not_installed("lme4")
  three_class <- iris
  three_class$fac <- rep(letters[1:3], 50)
  three_class$logical <- rep(c(TRUE, FALSE), 75)

  expect_snapshot(error = TRUE,
    recipe(Species ~ ., data = three_class) %>%
      step_lencode_mixed(Sepal.Length, outcome = vars(Species)) %>%
      prep(training = three_class, retain = TRUE)
  )
})


test_that("printing", {
  skip_if_not_installed("lme4")
  print_test <- recipe(x2 ~ ., data = ex_dat_ch) %>%
    step_lencode_mixed(x3, outcome = vars(x2))
  expect_snapshot(print_test)
  expect_snapshot(prep(print_test))
})

# ------------------------------------------------------------------------------

test_that("empty selections", {
  data(ad_data, package = "modeldata")
  expect_error(
    rec <-
      recipe(Class ~ Genotype + tau, data = ad_data) %>%
      step_lencode_mixed(starts_with("potato"), outcome = vars(Class)) %>%
      prep(),
    regexp = NA
  )
  expect_equal(
    bake(rec, new_data = NULL),
    ad_data %>% select(Genotype, tau, Class)
  )
})

# ------------------------------------------------------------------------------

test_that("case weights", {
  skip_if_not_installed("lme4")
  
  wts_int <- rep(c(0, 1), times = c(100, 400))
  
  ex_dat_cw <- ex_dat %>%
    mutate(wts = importance_weights(wts_int))
  
  class_test <- recipe(x2 ~ ., data = ex_dat_cw) %>%
    step_lencode_mixed(x3, outcome = vars(x2), id = "id") %>%
    prep(training = ex_dat_cw, retain = TRUE)
  
  ref_mod <- lme4::glmer(
    formula = y ~ 1 + (1 | x3),
    data = ex_dat_cw %>% mutate(y = as.numeric(x2) - 1), 
    family = stats::binomial,
    verbose = 0,
    na.action = na.omit,
    weights = wts_int
  )

  expect_equal(
    -coef(ref_mod)$x3[[1]],
    slice_head(class_test$steps[[1]]$mapping$x3, n = -1)$..value
  )
  
  expect_snapshot(class_test)
})

