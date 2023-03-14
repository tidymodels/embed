source(testthat::test_path("make_example_data.R"))
source(testthat::test_path("test_helpers.R"))

# ------------------------------------------------------------------------------

test_that("factor encoded predictor", {
  skip_on_cran()
  skip_if(!is_tf_available())

  class_test <- recipe(x2 ~ ., data = ex_dat) %>%
    step_embed(x3, outcome = vars(x2), options = embed_control(verbose = 0), id = "id") %>%
    prep(training = ex_dat, retain = TRUE)
  tr_values <- bake(class_test, new_data = NULL, contains("embed"))
  new_values <- bake(class_test, new_data = new_dat, contains("embed"))
  expect_snapshot(
    new_values_ch <- bake(class_test, new_data = new_dat_ch, contains("embed"))
  )

  key <- class_test$steps[[1]]$mapping
  td_obj <- tidy(class_test, number = 1)

  expect_equal("x3", names(key))

  expect_equal(
    length(unique(ex_dat$x3)) + 1,
    nrow(key$x3)
  )
  expect_equal(3, ncol(key$x3))

  expect_true(sum(key$x3$..level == "..new") == 1)

  expect_true(all(vapply(tr_values, is.numeric, logical(1))))

  expect_equal(
    new_values[1, ] %>% setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values[2, ] %>% setNames(letters[1:2]),
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -3] %>% setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values[3, ] %>% setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2]),
    ignore_attr = TRUE
  )

  expect_equal(
    new_values_ch[1, ] %>% setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values_ch[2, ] %>% setNames(letters[1:2]),
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -3] %>% setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values_ch[3, ] %>% setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2]),
    ignore_attr = TRUE
  )

  expect_equal(
    td_obj$level,
    key$x3$..level
  )
  expect_equal(
    td_obj %>% select(contains("emb")) %>% setNames(letters[1:2]),
    key$x3 %>% select(contains("emb")) %>% setNames(letters[1:2])
  )
})


test_that("character encoded predictor", {
  skip_on_cran()
  skip_if(!is_tf_available())

  class_test <- recipe(x2 ~ ., data = ex_dat_ch) %>%
    step_embed(x3, outcome = vars(x2), options = embed_control(verbose = 0)) %>%
    prep(training = ex_dat_ch, retain = TRUE)
  tr_values <- bake(class_test, new_data = NULL, contains("embed"))
  new_values <- bake(class_test, new_data = new_dat, contains("embed"))
  new_values_fc <- bake(class_test, new_data = new_dat, contains("embed"))

  key <- class_test$steps[[1]]$mapping
  td_obj <- tidy(class_test, number = 1)

  expect_equal("x3", names(key))

  expect_equal(
    length(unique(ex_dat$x3)) + 1,
    nrow(key$x3)
  )
  expect_equal(3, ncol(key$x3))

  expect_true(sum(key$x3$..level == "..new") == 1)

  expect_true(all(vapply(tr_values, is.numeric, logical(1))))

  expect_equal(
    new_values[1, ] %>% setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values[2, ] %>% setNames(letters[1:2]),
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -3] %>% setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values[3, ] %>% setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2]),
    ignore_attr = TRUE
  )

  expect_equal(
    new_values_fc[1, ] %>% setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values_fc[2, ] %>% setNames(letters[1:2]),
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -3] %>% setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values_fc[3, ] %>% setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2]),
    ignore_attr = TRUE
  )

  expect_equal(
    td_obj$level,
    key$x3$..level
  )
  expect_equal(
    td_obj %>% select(contains("emb")) %>% setNames(letters[1:2]),
    key$x3 %>% select(contains("emb")) %>% setNames(letters[1:2])
  )
})

# ------------------------------------------------------------------------------

test_that("factor encoded predictor", {
  skip_on_cran()
  skip_if(!is_tf_available())

  class_test <- recipe(x1 ~ ., data = ex_dat) %>%
    step_embed(x3, outcome = vars(x1), options = embed_control(verbose = 0)) %>%
    prep(training = ex_dat, retain = TRUE)
  tr_values <- bake(class_test, new_data = NULL, contains("embed"))
  new_values <- bake(class_test, new_data = new_dat, contains("embed"))
  expect_snapshot(
    new_values_ch <- bake(class_test, new_data = new_dat_ch, contains("embed"))
  )
  key <- class_test$steps[[1]]$mapping
  td_obj <- tidy(class_test, number = 1)

  expect_equal("x3", names(key))

  expect_equal(
    length(unique(ex_dat$x3)) + 1,
    nrow(key$x3)
  )
  expect_equal(3, ncol(key$x3))

  expect_true(sum(key$x3$..level == "..new") == 1)

  expect_true(all(vapply(tr_values, is.numeric, logical(1))))

  expect_equal(
    new_values[1, ] %>% setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values[2, ] %>% setNames(letters[1:2]),
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -3] %>% setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values[3, ] %>% setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2]),
    ignore_attr = TRUE
  )

  expect_equal(
    new_values_ch[1, ] %>% setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values_ch[2, ] %>% setNames(letters[1:2]),
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -3] %>% setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values_ch[3, ] %>% setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2]),
    ignore_attr = TRUE
  )

  expect_equal(
    td_obj$level,
    key$x3$..level
  )
  expect_equal(
    td_obj %>% select(contains("emb")) %>% setNames(letters[1:2]),
    key$x3 %>% select(contains("emb")) %>% setNames(letters[1:2])
  )
})


test_that("character encoded predictor", {
  skip_on_cran()
  skip_if(!is_tf_available())

  class_test <- recipe(x1 ~ ., data = ex_dat_ch) %>%
    step_embed(x3, outcome = vars(x1), num_terms = 5, options = embed_control(verbose = 0)) %>%
    prep(training = ex_dat_ch, retain = TRUE)
  tr_values <- bake(class_test, new_data = NULL, contains("embed"))
  new_values <- bake(class_test, new_data = new_dat, contains("embed"))
  new_values_fc <- bake(class_test, new_data = new_dat, contains("embed"))

  key <- class_test$steps[[1]]$mapping
  td_obj <- tidy(class_test, number = 1)

  expect_equal("x3", names(key))

  expect_equal(
    length(unique(ex_dat$x3)) + 1,
    nrow(key$x3)
  )
  expect_equal(6, ncol(key$x3))

  expect_true(sum(key$x3$..level == "..new") == 1)

  expect_true(all(vapply(tr_values, is.numeric, logical(1))))

  expect_equal(
    new_values[1, ] %>% setNames(letters[1:5]),
    key$x3[key$x3$..level == "..new", -6] %>% setNames(letters[1:5]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values[2, ] %>% setNames(letters[1:5]),
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -6] %>% setNames(letters[1:5]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values[3, ] %>% setNames(letters[1:5]),
    key$x3[key$x3$..level == "..new", -6] %>% setNames(letters[1:5]),
    ignore_attr = TRUE
  )

  expect_equal(
    new_values_fc[1, ] %>% setNames(letters[1:5]),
    key$x3[key$x3$..level == "..new", -6] %>% setNames(letters[1:5]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values_fc[2, ] %>% setNames(letters[1:5]),
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -6] %>% setNames(letters[1:5]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values_fc[3, ] %>% setNames(letters[1:5]),
    key$x3[key$x3$..level == "..new", -6] %>% setNames(letters[1:5]),
    ignore_attr = TRUE
  )

  expect_equal(
    td_obj$level,
    key$x3$..level
  )
  expect_equal(
    td_obj %>% select(contains("emb")) %>% setNames(letters[1:5]),
    key$x3 %>% select(contains("emb")) %>% setNames(letters[1:5])
  )
})


# ------------------------------------------------------------------------------

test_that("bad args", {
  skip_on_cran()
  skip_if(!is_tf_available())

  three_class <- iris
  three_class$fac <- rep(letters[1:3], 50)
  three_class$logical <- rep(c(TRUE, FALSE), 75)

  expect_snapshot(
    error = TRUE,
    recipe(Species ~ ., data = three_class) %>%
      step_embed(Sepal.Length, outcome = vars(Species)) %>%
      prep(training = three_class, retain = TRUE)
  )
})

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_on_cran()
  skip_if(!is_tf_available())
  rec <- recipe(x2 ~ ., data = ex_dat) %>%
    step_embed(x3, outcome = vars(x2), options = embed_control(verbose = 0), id = "id") %>%
    update_role(x3, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)

  expect_error(
    bake(rec_trained, new_data = ex_dat[, -3]),
    class = "new_data_missing_column"
  )
})

test_that("printing", {
  skip_on_cran()
  skip_if(!is_tf_available())

  print_test <- recipe(x2 ~ ., data = ex_dat_ch) %>%
    step_embed(x3, outcome = vars(x2))
  expect_snapshot(print_test)
  expect_snapshot(prep(print_test))
})


# ------------------------------------------------------------------------------

test_that("empty selections", {
  data(ad_data, package = "modeldata")
  expect_error(
    rec <-
      recipe(Class ~ Genotype + tau, data = ad_data) %>%
      step_embed(starts_with("potato"), outcome = vars(Class)) %>%
      prep(),
    regexp = NA
  )
  expect_equal(
    bake(rec, new_data = NULL),
    ad_data %>% select(Genotype, tau, Class)
  )
})
