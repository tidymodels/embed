library(testthat)
library(dplyr)
library(rsample)

context("discretize_tree")

source(test_path("make_binned_data.R"))

# ------------------------------------------------------------------------------

data("credit_data", package = "modeldata")
data("okc", package = "modeldata")

# Data for classification problem testing
set.seed(42)
credit_data_split <- initial_split(credit_data, strata = "Status")
credit_data_train <- training(credit_data_split)
credit_data_test <- testing(credit_data_split)

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

# Data for regression problem testing (naive)
set.seed(1773)
okc_data_split <- initial_split(okc, strata = "age")
okc_data_train <- training(okc_data_split)
okc_data_test <- testing(okc_data_split)


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

test_that("run_xgboost for classification", {
  
  xgboost <- embed:::run_xgboost(
    xgb_credit_train,
    xgb_credit_test,
    .learn_rate = 0.3,
    .num_breaks = 10,
    .tree_depth = 1,
    .objective = "binary:logistic"
  )
  
  expect_output(print(xgboost))
  expect_equal(length(xgboost$params), 8)
  expect_equal(xgboost$nfeatures, 13)
  expect_equal(xgboost$params$tree_method, "hist")
  expect_equal(xgboost$params$objective, "binary:logistic")
  
})

test_that("run_xgboost for regression", {
  
  xgboost <- embed:::run_xgboost(
    xgb_okc_train,
    xgb_okc_test,
    .learn_rate = 0.3,
    .num_breaks = 10,
    .tree_depth = 1,
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
  xgb_binning <- embed:::xgb_binning(
    credit_data_train,
    "Status",
    "Seniority",
    prop = 0.80,
    learn_rate = 0.3,
    num_breaks = 10,
    tree_depth = 1
  )
  
  expect_output(print(xgb_binning))
  expect_equal(length(xgb_binning), 6)
  expect_type(xgb_binning, "double")
  
  # Target has missing values
  credit_data_na <- credit_data_train
  credit_data_na[1, "Status"] <- NA
  
  expect_error(
    embed:::xgb_binning(
      credit_data_na,
      "Status",
      "Seniority",
      prop = 0.80,
      learn_rate = 0.3,
      num_breaks = 10,
      tree_depth = 1
    ),
    "Outcome variable contains missing values."
  )
  
  # Target has more than three levels
  credit_data_levels <- credit_data_train
  credit_data_levels$Status <- as.character(credit_data_levels$Status)
  credit_data_levels[1, "Status"] <- "third_level"
  
  expect_error(
    embed:::xgb_binning(
      credit_data_levels,
      "Status",
      "Seniority",
      prop = 0.80,
      learn_rate = 0.3,
      num_breaks = 10,
      tree_depth = 1
    ),
    "Outcome variable needs to have two levels (binary classification).",
    fixed = TRUE
  )
  
  # Algorithm runs on a too small training set/ insufficient variation in data
  set.seed(2393)
  credit_data_small <- dplyr::sample_n(credit_data_train, 20)
  
  expect_error(
    embed:::xgb_binning(
      credit_data_small,
      "Status",
      "Seniority",
      prop = 0.70,
      learn_rate = 0.3,
      num_breaks = 10,
      tree_depth = 1
    ),
    "No splits could be calculated as there is inssuficient training data/ insufficient data variation for XgBoost. Consider increasing the prop parameter to devote more data to the inner training set."
  )
  
  # Insufficient datapoints in inner test set
  expect_error(
    embed:::xgb_binning(
      credit_data_small,
      "Status",
      "Seniority",
      prop = 0.90,
      learn_rate = 0.3,
      num_breaks = 10,
      tree_depth = 1
    ),
    "Too few observations in the inner test set. Consider decreasing the prop parameter."
  )
  
})

test_that("xgb_binning for regression", {
  
  # Usual case
  xgb_binning <- embed:::xgb_binning(
    okc_data_train,
    "age",
    "height",
    prop = 0.80,
    learn_rate = 0.3,
    num_breaks = 10,
    tree_depth = 1
  )
  
  expect_output(print(xgb_binning))
  expect_equal(length(xgb_binning), 7)
  expect_type(xgb_binning, "double")
  
  # Target has missing values
  okc_data_na <- okc_data_train
  okc_data_na[1, "age"] <- NA
  
  expect_error(
    embed:::xgb_binning(
      okc_data_na,
      "age",
      "height",
      prop = 0.80,
      learn_rate = 0.3,
      num_breaks = 10,
      tree_depth = 1
    ),
    "Outcome variable contains missing values."
  )
  
  # Algorithm runs on a too small training set/ insufficient variation in data
  set.seed(8134)
  okc_data_small <- dplyr::sample_n(okc_data_train, 10)
  
  expect_error(
    embed:::xgb_binning(
      okc_data_small,
      "age",
      "height",
      prop = 0.70,
      learn_rate = 0.3,
      num_breaks = 10,
      tree_depth = 1
    ),
    "No splits could be calculated as there is inssuficient training data/ insufficient data variation for XgBoost. Consider increasing the prop parameter to devote more data to the inner training set."
  )
  
  # Insufficient data points in inner test set
  expect_error(
    embed:::xgb_binning(
      credit_data_small,
      "Status",
      "Seniority",
      prop = 0.90,
      learn_rate = 0.3,
      num_breaks = 10,
      tree_depth = 1
    ),
    "Too few observations in the inner test set. Consider decreasing the prop parameter."
  )
  
})

test_that("step_discretize_tree for classification", {
  set.seed(8497)
  sim_tr <- sim_data_2class(1000)
  sim_te <- sim_data_2class(100)
  # General use
  xgb_rec <-
    recipe(class ~ ., data = sim_tr) %>%
    step_discretize_tree(all_predictors(), outcome = "class")
  
  set.seed(28)
  xgb_rec <- prep(xgb_rec, training = sim_tr)
  
  xgb_train_bins <- bake(xgb_rec, sim_tr)
  xgb_test_bins <- bake(xgb_rec, sim_tr)
  
  expect_output(print(xgb_train_bins))
  expect_output(print(xgb_test_bins))
  expect_equal(
    levels(xgb_train_bins$x),
    c("[-Inf,0.3023)", "[0.3023,0.6815)", "[0.6815, Inf]")
  )
  expect_equal(
    levels(xgb_train_bins$y),
    c("[-Inf,0.3212)", "[0.3212, Inf]")
  )
  
  expect_equal(
    levels(xgb_train_bins$x),
    levels(xgb_test_bins$x)
  )
  expect_equal(
    levels(xgb_train_bins$z),
    levels(xgb_test_bins$z)
  )
 
  
  # No numeric variables present
  predictors_non_numeric <- c(
    "Status", "Home", "Marital"
  )
  
  xgb_rec <- credit_data_train %>% 
    select(one_of(predictors_non_numeric)) %>% 
    recipe(Status ~ .) %>%
    step_medianimpute(all_numeric()) %>%
    step_discretize_tree(all_numeric(), outcome = "Status")
  
  expect_error(
    prep(xgb_rec, 
         credit_data_train %>% 
           select(one_of(predictors_non_numeric)))
    ,
    "No variables or terms were selected."
  )
  
  # Information about insufficient datapoints for Time predictor
  msg <- capture_message(
    recipe(Status ~ ., data = credit_data_train) %>%
      step_medianimpute(all_numeric()) %>%
      step_discretize_tree(all_numeric(), outcome = "Status") %>%
      prep(retain = TRUE)
  )
  
  expect_true(
    grepl(
      "Time",
      msg
    )
  )
  
})

test_that("step_discretize_tree for regression", {
  set.seed(8497)
  sim_tr <- sim_data_reg(1000)
  sim_te <- sim_data_reg(100)
  # General use
  xgb_rec <-
    recipe(y ~ ., data = sim_tr) %>%
    step_discretize_tree(all_predictors(), outcome = "y")
  
  xgb_rec <- prep(xgb_rec, training = sim_tr)
  
  xgb_train_bins <- bake(xgb_rec, sim_tr)
  xgb_test_bins <- bake(xgb_rec, sim_tr)
  
  expect_output(print(xgb_train_bins))
  expect_output(print(xgb_test_bins))
  expect_equal(
    levels(xgb_train_bins$x),
    c("[-Inf,0.292)", "[0.292,0.4192)", "[0.4192,0.5551)", "[0.5551,0.6834)", "[0.6834, Inf]")
  )
  expect_equal(
    levels(xgb_train_bins$z),
    c("[-Inf,0.07115)", "[0.07115,0.2061)", "[0.2061,0.3212)", "[0.3212,0.4386)", 
      "[0.4386,0.569)", "[0.569,0.6808)", "[0.6808,0.7849)", "[0.7849,0.9201)", "[0.9201, Inf]")
  )
  expect_equal(
    levels(xgb_train_bins$x),
    levels(xgb_test_bins$x)
  )
  expect_equal(
    levels(xgb_train_bins$z),
    levels(xgb_test_bins$z)
  )  
  
  # No numeric variables present
  predictors_non_numeric <- c(
    "age", "diet", "location"
  )
  
  xgb_rec <- okc_data_train %>% 
    select(one_of(predictors_non_numeric)) %>% 
    recipe(age ~ .) %>%
    step_medianimpute(all_numeric()) %>%
    step_discretize_tree(all_numeric(), outcome = "age")
  
  # TODO what's going on here??
  expect_error(
    prep(xgb_rec, 
         okc_data_train %>% 
           select(one_of(predictors_non_numeric)))
    ,
    "No variables or terms were selected."
  )
  
})

test_that("printing", {
  
  xgb_rec <-
    recipe(Status ~ ., data = credit_data_train) %>%
    step_medianimpute(all_numeric()) %>%
    step_discretize_tree(all_numeric(), outcome = "Status")
  
  expect_output(print(xgb_rec))
  expect_output(prep(xgb_rec, xgb_rec = credit_data_train, verbose = TRUE))
  
})

test_that("tidy", {
  
  # TODO getting this weird error when running tidy"
  #  Error in dots_values(...) : object 'rules' not found 
  # I was trying to find it somewhere online but I can't really
  # find out why I'm getting it
  
  # General use
  xgb_rec <-
    recipe(Status ~ ., data = credit_data_train) %>%
    step_medianimpute(all_numeric()) %>%
    step_discretize_tree(all_numeric(), -Time, outcome = "Status") %>% 
    prep(training = credit_data_train)
  
  tidy(xgb_rec, number = 2)
  
})
