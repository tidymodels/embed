library(embed)
library(dplyr)
library(testthat)

source("make_example_data.R")

###################################################################

context("tensorflow model, classification")

test_that("factor encoded predictor", {
  skip_on_cran()
  class_test <- recipe(x2 ~ ., data = ex_dat) %>%
    step_tfembed(x3, outcome = vars(x2), options = tfembed_control(verbose = 0)) %>%
    prep(training = ex_dat, retain = TRUE)
  tr_values <- juice(class_test, contains("embed"))
  new_values <- bake(class_test, newdata = new_dat, contains("embed"))
  new_values_ch <- bake(class_test, newdata = new_dat_ch, contains("embed"))
  
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
  
  expect_equivalent(
    new_values[1,] %>% setNames(letters[1:2]), 
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2])
  )
  expect_equivalent(
    new_values[2,] %>% setNames(letters[1:2]), 
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -3] %>% setNames(letters[1:2])
  )  
  expect_equivalent(
    new_values[3,] %>% setNames(letters[1:2]), 
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2])
  )  
  
  expect_equivalent(
    new_values_ch[1,] %>% setNames(letters[1:2]), 
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2])
  )
  expect_equivalent(
    new_values_ch[2,] %>% setNames(letters[1:2]), 
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -3] %>% setNames(letters[1:2])
  )  
  expect_equivalent(
    new_values_ch[3,] %>% setNames(letters[1:2]), 
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2])
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
  class_test <- recipe(x2 ~ ., data = ex_dat_ch) %>%
    step_tfembed(x3, outcome = vars(x2), options = tfembed_control(verbose = 0)) %>%
    prep(training = ex_dat_ch, retain = TRUE)
  tr_values <- juice(class_test, contains("embed"))
  new_values <- bake(class_test, newdata = new_dat, contains("embed"))
  new_values_fc <- bake(class_test, newdata = new_dat, contains("embed"))
  
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
  
  expect_equivalent(
    new_values[1,] %>% setNames(letters[1:2]), 
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2])
  )
  expect_equivalent(
    new_values[2,] %>% setNames(letters[1:2]), 
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -3] %>% setNames(letters[1:2])
  )  
  expect_equivalent(
    new_values[3,] %>% setNames(letters[1:2]), 
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2])
  )  
  
  expect_equivalent(
    new_values_fc[1,] %>% setNames(letters[1:2]), 
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2])
  )
  expect_equivalent(
    new_values_fc[2,] %>% setNames(letters[1:2]), 
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -3] %>% setNames(letters[1:2])
  )  
  expect_equivalent(
    new_values_fc[3,] %>% setNames(letters[1:2]), 
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2])
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

###################################################################

context("tensorflow model, regression")

test_that("factor encoded predictor", {
  skip_on_cran()
  class_test <- recipe(x1 ~ ., data = ex_dat) %>%
    step_tfembed(x3, outcome = vars(x1), options = tfembed_control(verbose = 0)) %>%
    prep(training = ex_dat, retain = TRUE)
  tr_values <- juice(class_test, contains("embed"))
  new_values <- bake(class_test, newdata = new_dat, contains("embed"))
  new_values_ch <- bake(class_test, newdata = new_dat_ch, contains("embed"))
  
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
  
  expect_equivalent(
    new_values[1,] %>% setNames(letters[1:2]), 
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2])
  )
  expect_equivalent(
    new_values[2,] %>% setNames(letters[1:2]), 
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -3] %>% setNames(letters[1:2])
  )  
  expect_equivalent(
    new_values[3,] %>% setNames(letters[1:2]), 
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2])
  )  
  
  expect_equivalent(
    new_values_ch[1,] %>% setNames(letters[1:2]), 
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2])
  )
  expect_equivalent(
    new_values_ch[2,] %>% setNames(letters[1:2]), 
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -3] %>% setNames(letters[1:2])
  )  
  expect_equivalent(
    new_values_ch[3,] %>% setNames(letters[1:2]), 
    key$x3[key$x3$..level == "..new", -3] %>% setNames(letters[1:2])
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
  class_test <- recipe(x1 ~ ., data = ex_dat_ch) %>%
    step_tfembed(x3, outcome = vars(x1), number = 5, options = tfembed_control(verbose = 0)) %>%
    prep(training = ex_dat_ch, retain = TRUE)
  tr_values <- juice(class_test, contains("embed"))
  new_values <- bake(class_test, newdata = new_dat, contains("embed"))
  new_values_fc <- bake(class_test, newdata = new_dat, contains("embed"))
  
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
  
  expect_equivalent(
    new_values[1,] %>% setNames(letters[1:5]), 
    key$x3[key$x3$..level == "..new", -6] %>% setNames(letters[1:5])
  )
  expect_equivalent(
    new_values[2,] %>% setNames(letters[1:5]), 
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -6] %>% setNames(letters[1:5])
  )  
  expect_equivalent(
    new_values[3,] %>% setNames(letters[1:5]), 
    key$x3[key$x3$..level == "..new", -6] %>% setNames(letters[1:5])
  )  
  
  expect_equivalent(
    new_values_fc[1,] %>% setNames(letters[1:5]), 
    key$x3[key$x3$..level == "..new", -6] %>% setNames(letters[1:5])
  )
  expect_equivalent(
    new_values_fc[2,] %>% setNames(letters[1:5]), 
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -6] %>% setNames(letters[1:5])
  )  
  expect_equivalent(
    new_values_fc[3,] %>% setNames(letters[1:5]), 
    key$x3[key$x3$..level == "..new", -6] %>% setNames(letters[1:5])
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


###################################################################

context("tensorflow model, arguments")

test_that("bad args", {
  skip_on_cran()
  three_class <- iris
  three_class$fac <- rep(letters[1:3], 50)
  three_class$logical <- rep(c(TRUE, FALSE), 75)
  
  expect_error(
    recipe(Species ~ ., data = three_class) %>%
      step_tfembed(Sepal.Length, outcome = vars(Species)) %>%
      prep(training = three_class, retain = TRUE)
  )
})


test_that('printing', {
  skip_on_cran()
  print_test <- recipe(x2 ~ ., data = ex_dat_ch) %>%
    step_tfembed(x3, outcome = vars(x2)) 
  expect_output(print(print_test))
  expect_output(prep(print_test, training = ex_dat_ch, verbose = TRUE))
})


