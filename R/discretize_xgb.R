#' Discretize numeric variables with XgBoost
#'
#' `step_discretize_xgb()` creates a *specification* of a recipe step that will
#' discretize numeric data (e.g. integers or doubles) into bins in a supervised
#' way using an XgBoost model.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [recipes::selections] for more details.
#' @param role Defaults to `"predictor"`.
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated.
#' @param outcome A call to `vars` to specify which variable is used as the
#'   outcome to train XgBoost models in order to discretize explanatory
#'   variables.
#' @param sample_val Share of data used for validation (with early stopping) of
#'   the learned splits (the rest is used for training). Defaults to 0.20.
#' @param learn_rate The rate at which the boosting algorithm adapts from
#'   iteration-to-iteration. Corresponds to `eta` in the \pkg{xgboost} package.
#'   Defaults to 0.3.
#' @param num_breaks The _maximum_ number of discrete bins to bucket continuous
#'   features. Corresponds to `max_bin` in the \pkg{xgboost} package. Defaults
#'   to 10.
#' @param tree_depth The maximum depth of the tree (i.e. number of splits).
#'   Corresponds to `max_depth` in the \pkg{xgboost} package. Defaults to 1.
#' @param min_n The minimum number of instances needed to be in each node.
#'   Corresponds to `min_child_weight` in the \pkg{xgboost} package. Defaults to
#'   5.
#' @param rules The splitting rules of the best XgBoost tree to retain for each
#'   variable.
#' @param id A character string that is unique to this step to identify it.
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   [recipes::bake()]? While all operations are baked when [recipes::prep()] is
#'   run, some operations may not be able to be conducted on new data (e.g.
#'   processing the outcome variable(s)). Care should be taken when using `skip
#'   = TRUE` as it may affect the computations for subsequent operations
#' @template step-return
#' @details
#'
#' `step_discretize_xgb()` creates non-uniform bins from numerical variables by
#' utilizing the information about the outcome variable and applying the xgboost
#' model. It is advised to impute missing values before this step. This step is
#' intended to be used particularly with linear models because thanks to
#' creating non-uniform bins it becomes easier to learn non-linear patterns from
#' the data.
#'
#' The best selection of buckets for each variable is selected using an internal
#' early stopping scheme implemented in the \pkg{xgboost} package, which makes
#' this discretization method prone to overfitting.
#'
#' The pre-defined values of the underlying xgboost learns good and reasonably
#' complex results. However, if one wishes to tune them the recommended path
#' would be to first start with changing the value of `num_breaks` to e.g.: 20
#' or 30. If that doesn't give satisfactory results one could experiment with
#' modifying the `tree_depth` or `min_n` parameters. Note that it is not
#' recommended to tune `learn_rate` simultaneously with other parameters.
#'
#' This step requires the \pkg{xgboost} package. If not installed, the step will
#' stop with a note about installing the package.
#'
#' Note that the original data will be replaced with the new bins.
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe] this step, a tibble is returned with
#' columns `terms`, `value`, and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{value}{numeric, location of the splits}
#'   \item{id}{character, id of this step}
#' }
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_discretize_xgb"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-supervised
#'
#' @examplesIf rlang::is_installed(c("xgboost", "modeldata"))
#' library(rsample)
#' library(recipes)
#' data(credit_data, package = "modeldata")
#'
#' set.seed(1234)
#' split <- initial_split(credit_data[1:1000, ], strata = "Status")
#'
#' credit_data_tr <- training(split)
#' credit_data_te <- testing(split)
#'
#' xgb_rec <-
#'   recipe(Status ~ Income + Assets, data = credit_data_tr) |>
#'   step_impute_median(Income, Assets) |>
#'   step_discretize_xgb(Income, Assets, outcome = "Status")
#'
#' xgb_rec <- prep(xgb_rec, training = credit_data_tr)
#'
#' bake(xgb_rec, credit_data_te, Assets)
#' @seealso [embed::step_discretize_cart()], [recipes::recipe()],
#' [recipes::prep()], [recipes::bake()]
#' @export
step_discretize_xgb <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    outcome = NULL,
    sample_val = 0.20,
    learn_rate = 0.3,
    num_breaks = 10,
    tree_depth = 1,
    min_n = 5,
    rules = NULL,
    skip = FALSE,
    id = rand_id("discretize_xgb")
  ) {
    if (is.null(outcome)) {
      cli::cli_abort("{.arg outcome} should select at least one column.")
    }

    recipes_pkg_check(required_pkgs.step_discretize_xgb())

    add_step(
      recipe,
      step_discretize_xgb_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        outcome = outcome,
        sample_val = sample_val,
        learn_rate = learn_rate,
        num_breaks = num_breaks,
        tree_depth = tree_depth,
        min_n = min_n,
        rules = rules,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }

step_discretize_xgb_new <-
  function(
    terms,
    role,
    trained,
    outcome,
    sample_val,
    learn_rate,
    num_breaks,
    tree_depth,
    min_n,
    rules,
    skip,
    id,
    case_weights
  ) {
    step(
      subclass = "discretize_xgb",
      terms = terms,
      role = role,
      trained = trained,
      outcome = outcome,
      sample_val = sample_val,
      learn_rate = learn_rate,
      num_breaks = num_breaks,
      tree_depth = tree_depth,
      min_n = min_n,
      rules = rules,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

run_xgboost <- function(
  .train,
  .test,
  .learn_rate,
  .num_breaks,
  .tree_depth,
  .min_n,
  .objective,
  .num_class
) {
  # Need to set an additional parameter (num_class) when perfoming
  # multi-classification
  if (.objective == "multi:softprob") {
    .params <- list(
      eta = .learn_rate,
      max_bin = .num_breaks,
      max_depth = .tree_depth,
      min_child_weight = .min_n,
      num_class = .num_class
    )
  } else {
    .params <- list(
      eta = .learn_rate,
      max_bin = .num_breaks,
      max_depth = .tree_depth,
      min_child_weight = .min_n
    )
  }

  xgboost::xgb.train(
    params = .params,
    nrounds = 100,
    data = .train,
    watchlist = list(
      train = .train,
      test = .test
    ),
    tree_method = "hist",
    early_stopping_rounds = 10,
    objective = .objective,
    verbose = 0,
    nthread = 1
  )
}

xgb_binning <- function(
  df,
  outcome,
  predictor,
  sample_val,
  learn_rate,
  num_breaks,
  tree_depth,
  min_n,
  wts = NULL,
  call = caller_env()
) {
  # Assuring correct types
  if (is.character(df[[outcome]])) {
    df[[outcome]] <- as.factor(df[[outcome]])
  }

  # Checking the number of levels of the outcome
  levels <- levels(df[[outcome]])

  # ----------------------------------------------------------------------------

  df <- df[colnames(df) %in% c(outcome, predictor)]
  df <- df[complete.cases(df), , drop = FALSE]

  if (length(levels) >= 3) {
    df[[outcome]] <- as.integer(df[[outcome]]) - 1
  }

  # Changes: sample_val now is a parameter with 0.20 as default. If sample_val
  # is equal to 0 then rsample returns it standard error: Error: `prop` must be
  # a number on (0, 1). If there are less than 2 observations in the test set
  # then an error is given

  # Changes: I also realized that results for a single column are not
  # reproducible given a training set because sampling is not persistent,
  # therefore I added a specific random seed here which makes the results much
  # more stable and not so much dependent on inner sampling
  split <- withr::with_seed(
    sample.int(10^6, 1),
    # suppressing rsample messages regarding stratification (regression)
    suppressWarnings(
      rsample::initial_split(df, prop = (1 - sample_val), strata = outcome)
    )
  )

  train <- rsample::training(split)
  test <- rsample::testing(split)

  if (!is.null(wts)) {
    wts <- as.double(wts)
    wts_train <- wts[split$in_id]
    wts_test <- wts[-split$in_id]
  } else {
    wts_train <- NULL
    wts_test <- NULL
  }

  # Defining the objective function
  if (is.null(levels)) {
    objective <- "reg:squarederror"
    xgb_train <- xgboost::xgb.DMatrix(
      data = as.matrix(train[[predictor]], ncol = 1),
      label = train[[outcome]],
      weight = wts_train,
      nthread = 1
    )

    xgb_test <- xgboost::xgb.DMatrix(
      data = as.matrix(test[[predictor]], ncol = 1),
      label = test[[outcome]],
      weight = wts_test,
      nthread = 1
    )
  } else {
    if (length(levels) == 2) {
      objective <- "binary:logistic"

      xgb_train <- xgboost::xgb.DMatrix(
        data = as.matrix(train[[predictor]], ncol = 1),
        label = ifelse(train[[outcome]] == levels[[1]], 0, 1),
        weight = wts_train,
        nthread = 1
      )

      xgb_test <- xgboost::xgb.DMatrix(
        data = as.matrix(test[[predictor]], ncol = 1),
        label = ifelse(test[[outcome]] == levels[[1]], 0, 1),
        weight = wts_test,
        nthread = 1
      )
    } else if (length(levels) >= 3) {
      objective <- "multi:softprob" # returning estimated probability
      num_class <- length(levels)

      xgb_train <- xgboost::xgb.DMatrix(
        data = as.matrix(train[[predictor]], ncol = 1),
        label = train[[outcome]],
        weight = wts_train,
        nthread = 1
      )

      xgb_test <- xgboost::xgb.DMatrix(
        data = as.matrix(test[[predictor]], ncol = 1),
        label = test[[outcome]],
        weight = wts_test,
        nthread = 1
      )
    } else {
      cli::cli_abort(
        c(
          "Outcome variable only has less than 2 levels.",
          "i" = "Doesn't conform to regresion or classification task."
        ),
        call = call
      )
    }
  }

  xgb_mdl <-
    try(
      withr::with_seed(
        sample.int(10^6, 1),
        run_xgboost(
          xgb_train,
          .test = xgb_test,
          .learn_rate = learn_rate,
          .num_breaks = num_breaks,
          .tree_depth = tree_depth,
          .min_n = min_n,
          .objective = objective,
          .num_class = num_class
        )
      ),
      silent = TRUE
    )

  if (inherits(xgb_mdl, "try-error")) {
    err <- conditionMessage(attr(xgb_mdl, "condition"))
    cli::cli_warn(
      "Failed to create {.fn step_discretize_xgb} tree for predictor 
      {.val {predictor}}, which was not binned. The error: {err}"
    )
    return(numeric(0))
  }

  # Changes: if there is insufficient training data/ variation then xgboost
  # model is constant and no splits will be returned. Additional check will
  # inform the user that the dataset is insufficient for this particular case
  # https://github.com/dmlc/xgboost/issues/2876
  # https://stackoverflow.com/questions/42670033/r-getting-non-tree-model-detected-this-function-can-only-be-used-with-tree-mo
  xgb_tree <- try(
    xgboost::xgb.model.dt.tree(
      model = xgb_mdl,
      trees = 1:xgb_mdl$best_iteration,
      use_int_id = TRUE
    ),
    silent = TRUE
  )

  if (inherits(xgb_tree, "try-error")) {
    err <- conditionMessage(attr(xgb_tree, "condition"))
    if (grepl("Non-tree model detected", err)) {
      cli::cli_warn(
        c(
          "{.fn step_discretize_xgb} failed for predictor {.val {predictor}}.",
          "i" = "This could be because the data have no trend or because
           the learning rate is too low (current value: {learn_rate}).",
          "i" = "The predictor was not binned."
        )
      )
    } else {
      cli::cli_warn(
        "step_discretize_xgb() failed to create a tree with error for predictor 
        {.val {predictor}}, which was not binned. The error: {err}"
      )
    }

    return(numeric(0))
  }

  # Changes: keep all iterations up to the best one. Otherwise, it only kept the
  # split in the final tree. Also, just save the unique splits instead of the
  # data frame.

  xgb_split <-
    xgb_tree |>
    tibble::as_tibble() |>
    dplyr::select(Node, Feature, Split, Yes, No, Missing) |>
    stats::na.omit() |>
    dplyr::distinct(Split) |>
    dplyr::arrange(Split) |>
    dplyr::pull(Split)

  xgb_split
}

#' @export
prep.step_discretize_xgb <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_number_decimal(x$sample_val, min = 0, max = 1, arg = "sample_val")
  check_number_decimal(x$learn_rate, min = 0, arg = "learn_rate")
  check_number_whole(x$num_breaks, min = 0, arg = "num_breaks")
  check_number_whole(x$tree_depth, min = 0, arg = "tree_depth")
  check_number_whole(x$min_n, min = 0, arg = "min_n")

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts)
  if (isFALSE(were_weights_used) || is.null(wts)) {
    wts <- NULL
  }

  if (length(col_names) > 0) {
    check_type(training[, col_names], types = c("double", "integer"))

    y_name <- recipes_eval_select(x$outcome, training, info)

    col_names <- col_names[col_names != y_name]

    test_size <- sum(complete.cases(training)) * x$sample_val

    if (floor(test_size) < 2) {
      cli::cli_abort(
        c(
          "Too few observations in the early stopping validation set.",
          "i" = "Consider increasing the {.arg sample_val} parameter."
        )
      )
    }

    # Changes: check for the minimum number of unique data points in the column
    # in order to run the step. Otherwise, take it out of col_names. I think
    # that num_unique = 20 is probably a good default
    num_unique <- purrr::map_int(
      training[, col_names],
      \(.x) length(unique(.x))
    )
    too_few <- num_unique < 20
    if (any(too_few)) {
      predictors <- paste0("'", col_names[too_few], "'", collapse = ", ")
      cli::cli_warn(
        c(
          "More than 20 unique training set values are required.",
          "i" = "Predictors {predictors} were not processed; 
                their original values will be used."
        )
      )
      col_names <- col_names[!too_few]
    }

    rules <- vector("list", length(col_names))

    for (i in seq_along(col_names)) {
      rules[[i]] <- xgb_binning(
        training,
        y_name,
        col_names[[i]],
        x$sample_val,
        x$learn_rate,
        x$num_breaks,
        x$tree_depth,
        x$min_n,
        wts
      )
    }

    has_splits <- purrr::map_lgl(rules, \(.x) length(.x) > 0)

    rules <- rules[has_splits]
    col_names <- col_names[has_splits]
    if (length(col_names) > 0) {
      names(rules) <- col_names
    }
  } else {
    rules <- list()
  }

  step_discretize_xgb_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outcome = x$outcome,
    sample_val = x$sample_val,
    learn_rate = x$learn_rate,
    num_breaks = x$num_breaks,
    tree_depth = x$tree_depth,
    min_n = x$min_n,
    rules = rules,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_discretize_xgb <- function(object, new_data, ...) {
  vars <- object$rules

  check_new_data(names(vars), object, new_data)

  for (i in seq_along(vars)) {
    if (length(vars[[i]]) > 0) {
      var <- names(vars)[[i]]
      binned_data <- new_data

      binned_data[[var]] <- cut(
        new_data[[var]],
        breaks = c(-Inf, object$rules[[i]], Inf),
        include.lowest = TRUE,
        right = FALSE,
        dig.lab = 4
      )

      recipes::check_name(binned_data, new_data, object)
      new_data <- binned_data
    }
  }
  new_data
}

#' @export
print.step_discretize_xgb <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "Discretizing variables using xgboost "
  print_step(
    names(x$rules),
    x$terms,
    x$trained,
    title,
    width,
    case_weights = x$case_weights
  )
  invisible(x)
}

#' @rdname step_discretize_xgb
#' @usage NULL
#' @export
tidy.step_discretize_xgb <- function(x, ...) {
  if (is_trained(x)) {
    num_splits <- purrr::map_int(x$rules, length)

    if (length(num_splits) > 0) {
      res <- tibble(
        terms = rep(names(x$rules), num_splits),
        value = unlist(x$rules, use.names = FALSE)
      )
    } else {
      res <- tibble(
        terms = character(),
        value = double()
      )
    }
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = NA_real_
    )
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.embed
#' @export
required_pkgs.step_discretize_xgb <- function(x, ...) {
  c("xgboost", "embed")
}

#' @export
#' @rdname tunable_embed
tunable.step_discretize_xgb <- function(x, ...) {
  tibble::tibble(
    name = c("sample_val", "learn_rate", "num_breaks", "tree_depth", "min_n"),
    call_info = list(
      list(pkg = "dials", fun = "validation_set_prop", range = c(0.05, 0.7)),
      list(pkg = "dials", fun = "learn_rate", range = log10(c(0.05, 0.3))),
      list(pkg = "dials", fun = "num_breaks", range = c(5, 30)),
      list(pkg = "dials", fun = "tree_depth"),
      list(pkg = "dials", fun = "min_n")
    ),
    source = "recipe",
    component = "step_discretize_xgb",
    component_id = x$id
  )
}
