#' Discretize numeric variables with XgBoost
#'
#' `step_discretize_tree` creates a *specification* of a recipe step that will
#'  discretize numeric data (e.g. integers or doubles) into bins in a
#'  supervised way using an XgBoost model.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [selections()] for more details.
#' @param role Defaults to `"predictor"`.
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated.
#' @param outcome A call to `vars` to specify which variable is used as the
#'  outcome to train XgBoost models in order to discretize explanatory
#'  variables.
#' @param prop The share of data used for training the splits (the rest is used
#' for early stopping). Defaults to 0.80.
#' @param learn_rate The rate at which the boosting algorithm adapts from
#'  iteration-to-iteration. Corresponds to 'eta' in the \pkg{xgboost} package.
#'  Defaults to 0.3.
#' @param num_breaks The maximum number of discrete bins to bucket continuous
#'  features. Corresponds to 'max_bin' in the \pkg{xgboost} package. Defaults to
#'  10.
#' @param tree_depth The maximum depth of the tree (i.e. number of splits).
#' Corresponds to 'max_depth' in the \pkg{xgboost} package. Defaults to 1.
#' @param rules The splitting rules of the best XgBoost tree to retain for
#'  each variable.
#' @return An updated version of `recipe` with the new step added to the
#'  sequence of existing steps (if any).
#' @keywords binning
#' @concept preprocessing
#' @concept xgboost
#' @concept discretization
#' @concept factors
#' @export
#' @details step_discretize_tree creates non-uniform bins from numerical
#'  variables by utilizing the information about the outcome variable and
#'  applying the xgboost model. It is advised to impute missing values before
#'  this step. This step is intended to be used particularly with linear models
#'  because thanks to creating non-uniform bins it becomes easier to learn
#'  non-linear patterns from the data.
#'
#'  The best selection of buckets for each variable is selected using
#'  an internal early stopping scheme implemented in the \pkg{xgboost}
#'  package, which makes this discretization method prone to overfitting.
#'
#' The pre-defined values of the underlying xgboost learns should give good
#'  and reasonably complex results. However, if one wishes to tune them the
#'  recommended path would be to first start with changing the value of
#'  'num_breaks' to e.g.: 20 or 30. If that doesn't give satisfactory results
#'  one could experiment with increasing the 'tree_depth' parameter.
#'
#' This step requires the \pkg{xgboost} package. If not installed, the
#'  step will stop with a note about installing the package.
#'
#' Note that the original variables will be replaced with the new bins.
#'  The new variables will have names that begin with `prefix` followed
#'  by their original name.
#'
#' @examples
#' library(modeldata)
#' data(credit_data)
#' library(rsample)
#'
#' split <- initial_split(credit_data, strata = "Status")
#'
#' credit_data_tr <- training(split)
#' credit_data_te <- testing(split)
#'
#' xgb_rec <- 
#'   recipe(Status ~ ., data = credit_data_tr) %>%
#'   step_medianimpute(all_numeric()) %>%
#'   step_discretize_tree(all_numeric(), outcome = "Status")
#'
#' xgb_rec <- prep(xgb_rec, training = credit_data_tr)
#'
#' xgb_test_bins <- bake(xgb_rec, credit_data_te)
#' @seealso [recipe()] [prep.recipe()] [bake.recipe()]

step_discretize_tree <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           outcome = NULL,
           prop = 0.80,
           learn_rate = 0.3,
           num_breaks = 10, # I actually think it's a reasonable, minimal default as 
           # the XgBoost uses 256 as default. Furthermore, this parameter is defined as the maximum
           # number of bins - smaller number could be chosen by the algorithm
           tree_depth = 1,
           rules = NULL,
           skip = FALSE,
           id = rand_id("discretize_tree")) {
    
    if (is.null(outcome))
      rlang::abort("`outcome` should select at least one column.")

    add_step(
      recipe,
      step_discretize_tree_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        outcome = outcome,
        prop = prop,
        learn_rate = learn_rate,
        num_breaks = num_breaks,
        tree_depth = tree_depth,
        rules = rules,
        skip = skip,
        id = id
      )
    )
  }

step_discretize_tree_new <-
  function(terms, role, trained, outcome, prop, learn_rate, num_breaks,
           tree_depth, rules, skip, id) {
    step(
      subclass = "discretize_tree",
      terms = terms,
      role = role,
      trained = trained,
      outcome = outcome,
      prop = prop,
      learn_rate = learn_rate,
      num_breaks = num_breaks,
      tree_depth = tree_depth,
      rules = rules,
      skip = skip,
      id = id
    )
  }

run_xgboost <- function(.train, .test, .learn_rate, .num_breaks, .tree_depth, .objective){
  xgboost::xgb.train(
    params = list(
      eta = .learn_rate,
      max_bin = .num_breaks,
      max_depth = .tree_depth,
      min_child_weight = 5
    ),
    nrounds = 100, # TODO do you think nrounds and early_stopping_rounds are high enough? 
    # Should we expose them as parameters to the user?
    data = .train,
    watchlist = list(
      train = .train,
      test  = .test
    ),
    tree_method = "hist",
    early_stopping_rounds = 10,
    objective = .objective,
    verbose = 0,
    nthread = 1
  )
}

xgb_binning <- function(df, outcome, predictor, prop, learn_rate, num_breaks, tree_depth){
  
  # Assuring correct types
  if (is.character(df[[outcome]])) {
    df[[outcome]] <- as.factor(df[[outcome]])
  }
  
  # Checking the number of levels of the outcome
  levels <- levels(df[[outcome]])
  
  if (any(is.na(df[[outcome]]))) {
    rlang::abort("Outcome variable contains missing values.")
  }
  
  # ----------------------------------------------------------------------------
  
  # I added this as I realized that before I wasn't asserting that the algo runs on each numeric column individually
  df <- df[colnames(df) %in% c(outcome, predictor)]
  
  # Changes: prop now is a parameter with 0.80 as default. If prop is equal 
  # to 1 then rsample returns it standard error: Error: `prop` must be a number on (0, 1).
  # If there are less than 2 observations in the test set then an error is given
  
  # Changes: I also realized that results for a single column are not reproducable given a training set
  # because sampling is not persistent, therefore I added a specific random seed here
  # which makes the results much more stable and not so much dependent on inner sampling
  split <- withr:::with_seed(
    42,
    # suppressing rsample messages regarding stratification (regression)
    suppressWarnings(rsample::initial_split(df, prop = prop, strata = outcome))
  )

  train <- rsample::training(split)
  test  <- rsample::testing(split)
  
  if (nrow(test) < 2){
    rlang::abort("Too few observations in the inner test set. Consider decreasing the prop parameter.")
  }
  
  xgb_rec <- train %>% 
    dplyr::select(-one_of(outcome)) %>% 
    recipe(~ .) %>% 
    step_integer(all_predictors()) %>% 
    prep(retain = TRUE)
  
  # Defining the objective function
  if (is.null(levels)) {
    objective <- "reg:squarederror"
    xgb_train <- xgboost::xgb.DMatrix(
      data = as.matrix(juice(xgb_rec)), # this approach is needed to ensure all variables are represented as integers
      label = train[[outcome]]
    )
    
    xgb_test <- xgboost::xgb.DMatrix(
      data = as.matrix(bake(xgb_rec, new_data = test)),
      label = test[[outcome]]
    )
    
  } else {
    
    if (length(levels) == 2) {
      objective <- "binary:logistic"
    } else {
      rlang::abort("Outcome variable needs to have two levels (binary classification).")
    }
    xgb_train <- xgboost::xgb.DMatrix(
      data = as.matrix(juice(xgb_rec)),
      label = ifelse(train[[outcome]] == levels[[1]], 0, 1)
    )
    
    xgb_test <- xgboost::xgb.DMatrix(
      data = as.matrix(bake(xgb_rec, new_data = test)),
      label = ifelse(test[[outcome]] == levels[[1]], 0, 1)
    )
  }
  
  xgb_mdl <- withr:::with_seed(
    sample.int(10^6, 1),
    run_xgboost(
        xgb_train,
        .test = xgb_test,
        .learn_rate = learn_rate,
        .num_breaks = num_breaks,
        .tree_depth = tree_depth,
        .objective = objective
      )
  )
  
  # Changes: if there is insufficient training data/ variation then xgboost model is constant
  # and no splits will be returned. Additional check will inform the user
  # that the dataset is insufficient for this particular case
  # https://github.com/dmlc/xgboost/issues/2876
  # https://stackoverflow.com/questions/42670033/r-getting-non-tree-model-detected-this-function-can-only-be-used-with-tree-mo
  xgb_tree <- tryCatch(
    xgboost::xgb.model.dt.tree(
      model = xgb_mdl,
      trees = 1:xgb_mdl$best_iteration,
      use_int_id = TRUE
    ),
    error = function(e){
      "constant model"
    }
  )
  
  # TODO I'm not really happy with the way this is currently handled? Could you perhaps support here?
  if(all(length(xgb_tree) == 1, xgb_tree == "constant model")){
    rlang::abort(
      "No splits could be calculated as there is inssuficient training data/ insufficient data variation for XgBoost. Consider increasing the prop parameter to devote more data to the inner training set."
    )
  }
  
  # Changes: keep all iterations up to the best one. Otherwise, it only kept the
  # split in the final tree. Also, just save the unique splits instead of the 
  # data frame. 
  
  xgb_split <-
    xgb_tree %>%
    tibble::as_tibble() %>%
    dplyr::select(Node, Feature, Split, Yes, No, Missing) %>% 
    stats::na.omit() %>% 
    dplyr::distinct(Split) %>% 
    dplyr::arrange(Split) %>% 
    dplyr::pull(Split)
  
  xgb_split
}

#' @export
prep.step_discretize_tree <- function(x, training, info = NULL, ...) {
  
  col_names <- recipes::terms_select(terms = x$terms, info = info)
  check_type(training[, col_names])
  
  y_name <- recipes::terms_select(terms = x$outcome, info = info)
  
  col_names <- col_names[col_names != y_name]
  
  # Changes: check for the minimum number of unique data points in the column
  # in order to run the step. Otherwise, take it out of col_names. I think that 
  # num_unique = 20 is probably a good default
  col_names_removed <- NULL
  for (i in seq_along(col_names)){
    if(length(unique(unlist(training[, col_names[[i]]]))) < 20){
      col_names_removed <- c(col_names_removed, col_names[[i]])
    }
  }
  col_names <- col_names[!(col_names %in% col_names_removed)]
  
  if (length(col_names_removed) > 0){
    message(
      paste0(
      "The following columns were removed from processing due to insufficient number of unique observations (minimum 20): ", 
      paste(col_names_removed, sep = "", collapse = ", "), "."
      )
    )
  }
  
  rules <- vector("list", length(col_names))
  
  for (i in seq_along(col_names)) {
    rules[[i]] <- xgb_binning(
      training,
      y_name,
      col_names[[i]],
      x$prop,
      x$learn_rate,
      x$num_breaks,
      x$tree_depth
      )
  }
  
  names(rules) <- col_names
  
  step_discretize_tree_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outcome = x$outcome,
    prop = x$prop,
    learn_rate = x$learn_rate,
    num_breaks = x$num_breaks,
    tree_depth = x$tree_depth,
    rules = rules,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_discretize_tree <- function(object, new_data, ...) {
  vars <- object$rules
  
  for (i in seq_along(vars)) {
    var <- names(vars)[[i]]
    
    # Not sure what you mean?
    
    binned_data <- new_data
    
    binned_data[, var] <- cut(
      new_data[[var]],
      breaks = c(-Inf, object$rules[[i]], Inf),
      include.lowest = TRUE,
      right = FALSE,
      dig.lab = 4  
    )
    
    recipes::check_name(binned_data, new_data, object)
    new_data <- binned_data
  }
  tibble::as_tibble(new_data)
}

print.step_discretize_tree <- function(x, width = max(20, options()$width - 30), ...) {
    cat("Discretizing variables using XgBoost ")
    printer(names(x$rules), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_discretize_tree
#' @param x A `step_discretize_tree` object.
#' @export
tidy.step_discretize_tree <- function(x, ...) {
  print("hello?")
  # if (is_trained(x)) {
  #   res <- tibble("variable" = NA, "values" = NA)
  #   for(i in seq_along(x$col_names))
  #     print(x$col_names)
  #     rules <- expand_grid("variable" = x$col_names[[i]], "values" = x$rules[[i]])
  #     res <- bind_rows(res, rules)
  #   } else {
  #   term_names <- sel2char(x$terms)
  #   res <- tibble(variable = term_names,
  #                 values = rlang::na_chr)
  # }
  res$id <- x$id
  res
}

# ------------------------------------------------------------------------------

#' #' @importFrom utils packageVersion
#' tidyr_new_interface <- function() {
#'   utils::packageVersion("tidyr") > "0.8.99"
#' }


#' #' @importFrom utils globalVariables
#' utils::globalVariables(
#'   c("n", "p", "predictor", "summary_outcome", "value", "woe", "select", "variable", ".")
#' )
