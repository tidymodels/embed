source(testthat::test_path("make_example_data.R"))
source(testthat::test_path("test_helpers.R"))

opts <- list(seed = 34677, chains = 2, iter = 500)

omit_warning <- function(pattern) {
  function(x) {
    if (grepl(pattern, x[1])) {
      return(NULL)
    }
    x
  }
}

# ------------------------------------------------------------------------------

test_that("factor encoded predictor", {
  skip_on_cran()
  skip_if_not_installed("rstanarm")
  
  scrub_rhat_warning <- function(x) {
    if (grepl("^The largest R-hat is", x[1])) {
      return(character())
    }
    x
  }
  expect_snapshot(transform = scrub_rhat_warning, {
    class_test <- recipe(x2 ~ ., data = ex_dat) %>%
      step_lencode_bayes(x3,
                         outcome = vars(x2),
                         verbose = FALSE,
                         options = opts
      ) %>%
      prep(training = ex_dat, retain = TRUE)
  }
  )
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
  skip_on_cran()
  skip_if_not_installed("rstanarm")
  expect_snapshot(
    transform = omit_warning("^The largest R-hat is 1.05"),
    class_test <- recipe(x2 ~ ., data = ex_dat_ch) %>%
      step_lencode_bayes(x3,
        outcome = vars(x2),
        verbose = FALSE,
        options = opts,
        id = "id"
      ) %>%
      prep(
        training = ex_dat_ch, retain = TRUE,
        options = opts
      )
  )
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

# ------------------------------------------------------------------------------

test_that("factor encoded predictor", {
  skip_on_cran()
  skip_if_not_installed("rstanarm")
  expect_snapshot(
    transform = omit_warning("^Bulk Effective Samples Size"),
    {
    set.seed(8283)
    reg_test <- recipe(x1 ~ ., data = ex_dat) %>%
      step_lencode_bayes(x3,
        outcome = vars(x1),
        verbose = FALSE,
        options = opts
      ) %>%
      prep(training = ex_dat, retain = TRUE)
  })

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
  skip_on_cran()
  skip_if_not_installed("rstanarm")
  expect_snapshot(
    transform = omit_warning("^Bulk Effective Samples Size"),
    {
    set.seed(8283)
    reg_test <- recipe(x1 ~ ., data = ex_dat_ch) %>%
      step_lencode_bayes(x3,
        outcome = vars(x1),
        verbose = FALSE,
        options = opts
      ) %>%
      prep(training = ex_dat_ch, retain = TRUE)
  })

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



# ------------------------------------------------------------------------------

test_that("empty selections", {
  data(ad_data, package = "modeldata")
  expect_error(
    rec <-
      recipe(Class ~ Genotype + tau, data = ad_data) %>%
      step_lencode_bayes(starts_with("potato"), outcome = vars(Class)) %>%
      prep(),
    regexp = NA
  )
  expect_equal(
    bake(rec, new_data = NULL),
    ad_data %>% select(Genotype, tau, Class)
  )
})
