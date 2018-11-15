library(embed)
library(dplyr)
library(testthat)

source("make_example_data.R")

###################################################################

context("mixed model, classification")

test_that("factor encoded predictor", {
  class_test <- recipe(x2 ~ ., data = ex_dat) %>%
    step_lencode_mixed(x3, outcome = vars(x2), id = "id") %>%
    prep(training = ex_dat, retain = TRUE)
  tr_values <- juice(class_test)$x3
  new_values <- bake(class_test, new_data = new_dat)
  new_values_ch <- bake(class_test, new_data = new_dat_ch)
  
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
    key$x3$..value[ key$x3$..level == levels(ex_dat$x3)[1] ]
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
    key$x3$..value[ key$x3$..level == levels(ex_dat$x3)[1] ]
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
    key$x3$..value[ key$x3$..level == levels(ex_dat$x3)[1] ]
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
    key$x3$..value[ key$x3$..level == levels(ex_dat$x3)[1] ]
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

context("mixed model, regression")

test_that("factor encoded predictor", {
  reg_test <- recipe(x1 ~ ., data = ex_dat) %>%
    step_lencode_mixed(x3, outcome = vars(x1)) %>%
    prep(training = ex_dat, retain = TRUE)
  tr_values <- juice(reg_test)$x3
  new_values <- bake(reg_test, new_data = new_dat)
  new_values_ch <- bake(reg_test, new_data = new_dat_ch)
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
    key$x3$..value[ key$x3$..level == levels(ex_dat$x3)[1] ]
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
    key$x3$..value[ key$x3$..level == levels(ex_dat$x3)[1] ]
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
    key$x3$..value[ key$x3$..level == levels(ex_dat$x3)[1] ]
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
    key$x3$..value[ key$x3$..level == levels(ex_dat$x3)[1] ]
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

context("mixed model, arguments")

test_that("bad args", {
  three_class <- iris
  three_class$fac <- rep(letters[1:3], 50)
  three_class$logical <- rep(c(TRUE, FALSE), 75)
  
  expect_error(
    recipe(Species ~ ., data = three_class) %>%
      step_lencode_mixed(Sepal.Length, outcome = vars(Species)) %>%
      prep(training = three_class, retain = TRUE)
  )
})


test_that('printing', {
  print_test <- recipe(x2 ~ ., data = ex_dat_ch) %>%
    step_lencode_mixed(x3, outcome = vars(x2)) 
  expect_output(print(print_test))
  expect_output(prep(print_test, training = ex_dat_ch, verbose = TRUE))
})







