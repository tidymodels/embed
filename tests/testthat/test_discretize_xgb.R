library(testthat)
library(dplyr)
library(rsample)

context("discretize_xgb")

source(test_path("make_binned_data.R"))

data("credit_data", package = "modeldata")
data("okc", package = "modeldata")
data("attrition", package = "modeldata")

# ------------------------------------------------------------------------------

# Data for classification problem testing
set.seed(42)
credit_data_split <- initial_split(credit_data, strata = "Status")
credit_data_train <- training(credit_data_split)
credit_data_test <- testing(credit_data_split)

set.seed(2393)
credit_data_small <- dplyr::sample_n(credit_data_train, 20)

rec_credit <- credit_data_train %>%
  select(-Status) %>% 
  recipe(~ .) %>% 
  step_integer(all_predictors()) %>% 
  prep(retain = TRUE)

xgb_credit_train <- xgboost::xgb.DMatrix(
  data = as.matrix(juice(rec_credit)),
  label = ifelse(credit_data_train[["Status"]] == "bad", 0, 1)
)

xgb_credit_test <- xgboost::xgb.DMatrix(
  data = as.matrix(bake(rec_credit, new_data = credit_data_test)),
  label = ifelse(credit_data_test[["Status"]] == "bad", 0, 1)
)

# ------------------------------------------------------------------------------

# Data for multi-classification problem testing
set.seed(42)
attrition <- attrition %>% mutate(EducationField = as.integer(EducationField) - 1)
attrition_data_split <- initial_split(attrition, strata = "EducationField")
attrition_data_train <- training(attrition_data_split)
attrition_data_test <- testing(attrition_data_split)

set.seed(2393)
attrition_data_small <- dplyr::sample_n(attrition_data_train, 10)

rec_attrition <- attrition_data_train %>%
  select(-EducationField) %>% 
  recipe(~ .) %>% 
  step_integer(all_predictors()) %>% 
  prep(retain = TRUE)

xgb_attrition_train <- xgboost::xgb.DMatrix(
  data = as.matrix(juice(rec_attrition)),
  label = attrition_data_train$EducationField
)

xgb_attrition_test <- xgboost::xgb.DMatrix(
  data = as.matrix(bake(rec_attrition, new_data = attrition_data_test)),
  label = attrition_data_test$EducationField
)

# ------------------------------------------------------------------------------

# Data for regression problem testing (naive)
set.seed(1773)
okc_data_split <- initial_split(okc, strata = "age")
okc_data_train <- training(okc_data_split)
okc_data_test <- testing(okc_data_split)

set.seed(8134)
okc_data_small <- dplyr::sample_n(okc_data_train, 10)

rec_okc <- okc_data_train %>% 
  select(-age) %>% 
  recipe(~ .) %>% 
  step_integer(all_predictors()) %>% 
  prep(retain = TRUE)

xgb_okc_train <- xgboost::xgb.DMatrix(
  data = as.matrix(juice(rec_okc)),
  label = okc_data_train[["age"]]
)

xgb_okc_test <- xgboost::xgb.DMatrix(
  data = as.matrix(bake(rec_okc, new_data = okc_data_test)),
  label = okc_data_test[["age"]]
)

# ------------------------------------------------------------------------------

set.seed(8497)
sim_tr_cls <- sim_data_2class(1000)
sim_te_cls <- sim_data_2class(100)

set.seed(8497)
sim_tr_mcls <- sim_data_3class(1000)
sim_te_mcls <- sim_data_3class(100)

set.seed(8497)
sim_tr_reg <- sim_data_reg(1000)
sim_te_reg <- sim_data_reg(100)

# ------------------------------------------------------------------------------

test_that("run_xgboost for classification", {
  
  xgboost <- embed:::run_xgboost(
    xgb_credit_train,
    xgb_credit_test,
    .learn_rate = 0.3,
    .num_breaks = 10,
    .tree_depth = 1,
    .min_n = 5,
    .objective = "binary:logistic"
  )
  
  expect_output(print(xgboost))
  expect_equal(length(xgboost$params), 8)
  expect_equal(xgboost$nfeatures, 13)
  expect_equal(xgboost$params$tree_method, "hist")
  expect_equal(xgboost$params$objective, "binary:logistic")
  
})

test_that("run_xgboost for multi-classification", {
  
  xgboost <- embed:::run_xgboost(
    xgb_attrition_train,
    xgb_attrition_test,
    .learn_rate = 0.3,
    .num_breaks = 10,
    .tree_depth = 1,
    .min_n = 5,
    .num_class = 6, # label must be in [0, num_class)
    .objective = "multi:softprob"
  )
  
  expect_output(print(xgboost))
  expect_equal(length(xgboost$params), 9)
  expect_equal(xgboost$nfeatures, 30)
  expect_equal(xgboost$params$tree_method, "hist")
  expect_equal(xgboost$params$objective, "multi:softprob")
  
})

test_that("run_xgboost for regression", {
  
  xgboost <- embed:::run_xgboost(
    xgb_okc_train,
    xgb_okc_test,
    .learn_rate = 0.3,
    .num_breaks = 10,
    .tree_depth = 1,
    .min_n = 5,
    .objective = "reg:squarederror"
  )
  
  expect_output(print(xgboost))
  expect_equal(length(xgboost$params), 8)
  expect_equal(xgboost$nfeatures, 5)
  expect_equal(xgboost$params$tree_method, "hist")
  expect_equal(xgboost$params$objective, "reg:squarederror")
  
})

test_that("xgb_binning for classification", {
  
  # Usual case
  set.seed(8497)
  xgb_binning <- embed:::xgb_binning(
    credit_data_train,
    "Status",
    "Seniority",
    sample_val = 0.20,
    learn_rate = 0.3,
    num_breaks = 10,
    tree_depth = 1,
    min_n = 5
  )
  
  expect_output(print(xgb_binning))
  expect_equal(length(xgb_binning), 6)
  expect_type(xgb_binning, "double")
  
  # Algorithm runs on a too small training set/ insufficient variation in data

  expect_warning(
    embed:::xgb_binning(
      credit_data_small,
      "Status",
      "Seniority",
      sample_val = 0.30,
      learn_rate = 0.3,
      num_breaks = 10,
      tree_depth = 1,
      min_n = 5
    ),
    "failed for predictor 'Seniority'"
  )
  
})

test_that("xgb_binning for multi-classification", {
  
  # Usual case
  set.seed(8497)
  xgb_binning <- embed:::xgb_binning(
    attrition_data_train,
    "EducationField",
    "Age",
    sample_val = 0.20,
    learn_rate = 0.3,
    num_breaks = 10,
    tree_depth = 1,
    min_n = 5
  )
  
  expect_output(print(xgb_binning))
  expect_equal(length(xgb_binning), 6)
  expect_type(xgb_binning, "double")
  
  # Algorithm runs on a too small training set/ insufficient variation in data
  
  expect_warning(
    embed:::xgb_binning(
      attrition_data_small,
      "EducationField",
      "Age",
      sample_val = 0.30,
      learn_rate = 0.3,
      num_breaks = 10,
      tree_depth = 1,
      min_n = 5
    ),
    "failed for predictor 'Age'"
  )
  
})

test_that("xgb_binning for regression", {
  
  set.seed(4235)
  # Usual case
  xgb_binning <- embed:::xgb_binning(
    okc_data_train,
    "age",
    "height",
    sample_val = 0.20,
    learn_rate = 0.3,
    num_breaks = 10,
    tree_depth = 1,
    min_n = 5
  )
  
  expect_output(print(xgb_binning))
  expect_equal(length(xgb_binning), 6)
  expect_type(xgb_binning, "double")
  

  # Algorithm runs on a too small training set/ insufficient variation in data

  expect_warning(
    embed:::xgb_binning(
      okc_data_small,
      "age",
      "height",
      sample_val = 0.30,
      learn_rate = 0.3,
      num_breaks = 10,
      tree_depth = 1,
      min_n = 5
    ),
    "failed for predictor 'height'"
  )
  
})

test_that("step_discretize_xgb for classification", {
  set.seed(125)
  # General use
  xgb_rec <-
    recipe(class ~ ., data = sim_tr_cls) %>%
    step_discretize_xgb(all_predictors(), outcome = "class")
  
  set.seed(28)
  xgb_rec <- prep(xgb_rec, training = sim_tr_cls)
  
  xgb_train_bins <- bake(xgb_rec, sim_tr_cls)
  xgb_test_bins <- bake(xgb_rec, sim_te_cls)
  
  expect_output(print(xgb_train_bins))
  expect_output(print(xgb_test_bins))
  expect_equal(
    levels(xgb_train_bins$x),
    c("[-Inf,0.2982)", "[0.2982,0.6828)", "[0.6828, Inf]")
  )
  expect_equal(
    levels(xgb_train_bins$z),
    c("[-Inf,0.3191)", "[0.3191, Inf]")
  )
  
  expect_equal(
    levels(xgb_train_bins$x),
    levels(xgb_test_bins$x)
  )
  expect_equal(
    levels(xgb_train_bins$z),
    levels(xgb_test_bins$z)
  )
 
  # Too few data
  expect_error(
    recipe(class ~ ., data = sim_tr_cls[1:9, ]) %>%
      step_discretize_xgb(all_predictors(), outcome = "class") %>% 
      prep(),
    "Too few observations in the early stopping validation set"
  )
  
  # No numeric variables present
  predictors_non_numeric <- c(
    "Status", "Home", "Marital"
  )
  
  xgb_rec <- credit_data_train %>% 
    select(one_of(predictors_non_numeric)) %>% 
    recipe(Status ~ .) %>%
    step_medianimpute(all_numeric()) %>%
    step_discretize_xgb(all_numeric(), outcome = "Status")
  
  expect_error(
    prep(xgb_rec, 
         credit_data_train %>% 
           select(one_of(predictors_non_numeric)))
    ,
    "No variables or terms were selected."
  )
  
  # Information about insufficient datapoints for Time predictor
  msg <- capture_warning(
    recipe(Status ~ ., data = credit_data_train) %>%
      step_medianimpute(all_numeric()) %>%
      step_discretize_xgb(all_numeric(), outcome = "Status") %>%
      prep(retain = TRUE)
  )
  
  expect_true(
    grepl(
      "More than 20 unique training set values are required. Predictors 'Time' were not processed; their original values will be used.",
      msg
    )
  )
  
})

test_that("step_discretize_xgb for multi-classification", {
  set.seed(125)
  # General use
  xgb_rec <-
    recipe(class ~ ., data = sim_tr_mcls) %>%
    step_discretize_xgb(all_predictors(), outcome = "class")
  
  set.seed(28)
  xgb_rec <- prep(xgb_rec, training = sim_tr_mcls)
  
  xgb_train_bins <- bake(xgb_rec, sim_tr_mcls)
  xgb_test_bins <- bake(xgb_rec, sim_te_mcls)
  
  expect_output(print(xgb_train_bins))
  expect_output(print(xgb_test_bins))
  expect_equal(
    levels(xgb_train_bins$x),
    c("[-Inf,0.6806)", "[0.6806, Inf]")
  )
  expect_equal(
    levels(xgb_train_bins$z),
    c("[-Inf,0.3249)", "[0.3249, Inf]")
  )
  
  expect_equal(
    levels(xgb_train_bins$x),
    levels(xgb_test_bins$x)
  )
  expect_equal(
    levels(xgb_train_bins$z),
    levels(xgb_test_bins$z)
  )
  
  # Too few data
  expect_error(
    recipe(class ~ ., data = sim_tr_mcls[1:9, ]) %>%
      step_discretize_xgb(all_predictors(), outcome = "class") %>% 
      prep(),
    "Too few observations in the early stopping validation set"
  )
  
  
  # No numeric variables present
  predictors_non_numeric <- c(
    "Attrition", "BusinessTravel", "Department",
    "Education", "EnvironmentSatisfaction", "Gender",
    "JobInvolvement", "JobRole", "JobSatisfaction",
    "MaritalStatus", "OverTime", "PerformanceRating",
    "RelationshipSatisfaction", "WorkLifeBalance"
  )
  
  xgb_rec <- attrition_data_train %>% 
    select(one_of(predictors_non_numeric)) %>% 
    recipe(BusinessTravel ~ .) %>%
    step_medianimpute(all_numeric()) %>%
    step_discretize_xgb(all_numeric(), outcome = "BusinessTravel")
  
  expect_error(
    prep(xgb_rec, 
         attrition_data_train %>% 
           select(one_of(predictors_non_numeric)))
    ,
    "No variables or terms were selected."
  )
  
})

test_that("step_discretize_xgb for regression", {
  # Skip on R < 3.6 since the rng is different. 
  
  less_than_3.6 <- function() {
    utils::compareVersion("3.5.3", as.character(getRversion())) >= 0
  }
  skip_if(less_than_3.6())
  
  # General use
  set.seed(83834)
  xgb_rec <-
    recipe(y ~ ., data = sim_tr_reg) %>%
    step_discretize_xgb(all_predictors(), outcome = "y")
  tidy_untrained <- tidy(xgb_rec, 1)
  
  xgb_rec <- prep(xgb_rec, training = sim_tr_reg)
  tidy_trained <- tidy(xgb_rec, 1)
  
  xgb_train_bins <- bake(xgb_rec, sim_tr_reg)
  xgb_test_bins <- bake(xgb_rec, sim_te_reg)
  
  expect_output(print(xgb_train_bins))
  expect_output(print(xgb_test_bins))
  expect_equal(
    levels(xgb_train_bins$x),
    c("[-Inf,0.3069)", "[0.3069,0.4281)", "[0.4281,0.558)", "[0.558,0.6828)", "[0.6828, Inf]")
  )
  expect_equal(
    levels(xgb_train_bins$z),
    c("[-Inf,0.06851)", "[0.06851,0.2019)", "[0.2019,0.3192)", "[0.3192,0.4447)",
      "[0.4447,0.575)", "[0.575,0.789)", "[0.789,0.9222)", "[0.9222, Inf]")
  )
  expect_equal(
    levels(xgb_train_bins$x),
    levels(xgb_test_bins$x)
  )
  expect_equal(
    levels(xgb_train_bins$z),
    levels(xgb_test_bins$z)
  )  

  expect_true(tibble::is_tibble(tidy_untrained))
  expect_equal(
    tidy_untrained$variable,
    "all_predictors()"
  )  
  expect_equal(
    tidy_untrained$values,
    NA_character_
  ) 
  expect_true(tibble::is_tibble(tidy_trained))
  expect_equal(
    tidy_trained$terms,
    rep(c("x", "z"), c(4, 7))
  )  
  expect_equal(
    tidy_trained$values,
    unlist(xgb_rec$steps[[1]]$rules, use.names = FALSE)
  )  
  
  # one bad predictor
  sim_tr_reg$x <- round(sim_tr_reg$x, 1)
  expect_warning(
    xgb_rec <-
      recipe(y ~ ., data = sim_tr_reg[1:100,]) %>%
      step_discretize_xgb(all_predictors(), outcome = "y") %>% 
      prep(),
    "Predictors 'x' were not processed"
  )
  
  # No numeric variables present
  predictors_non_numeric <- c(
    "age", "diet", "location"
  )
  
  xgb_rec <- okc_data_train %>% 
    select(one_of(predictors_non_numeric)) %>% 
    recipe(age ~ .) %>%
    step_medianimpute(all_numeric()) %>%
    step_discretize_xgb(all_predictors(), outcome = "age")
  
  expect_error(
    prep(xgb_rec, 
         okc_data_train %>% 
           select(one_of(predictors_non_numeric))),
    "All columns selected for the step should be numeric"
  )
  
})

test_that("printing", {
  xgb_rec <-
    recipe(class ~ ., data = sim_tr_cls) %>%
    step_discretize_xgb(all_predictors(), outcome = "class")
  
  expect_output(print(xgb_rec))
  expect_output(prep(xgb_rec, verbose = TRUE))
})
