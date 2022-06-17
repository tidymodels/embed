#' Discretize numeric variables with CART
#'
#' `step_discretize_cart` creates a *specification* of a recipe step that will
#'  discretize numeric data (e.g. integers or doubles) into bins in a
#'  supervised way using a CART model.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [selections()] for more details.
#' @param role Defaults to `"predictor"`.
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated.
#' @param outcome A call to `vars` to specify which variable is used as the
#'  outcome to train CART models in order to discretize explanatory
#'  variables.
#' @param cost_complexity The regularization parameter. Any split that does not
#'  decrease the overall lack of fit by a factor of `cost_complexity` is not
#'  attempted. Corresponds to `cp` in [rpart::rpart()]. Defaults to 0.01.
#' @param tree_depth The _maximum_ depth in the final tree. Corresponds to
#'  `maxdepth` in  [rpart::rpart()]. Defaults to 10.
#' @param min_n The number of data points in a node required to continue
#'  splitting. Corresponds to `minsplit` in  [rpart::rpart()]. Defaults to 20.
#' @param rules The splitting rules of the best CART tree to retain for
#'  each variable. If length zero, splitting could not be used on that column.
#' @param id A character string that is unique to this step to identify it.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [recipes::bake()]? While all operations are baked
#'  when [recipes::prep()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#' @template step-return
#' @export
#' @details `step_discretize_cart()` creates non-uniform bins from numerical
#'  variables by utilizing the information about the outcome variable and
#'  applying a CART model.
#'
#'  The best selection of buckets for each variable is selected using
#'  the standard cost-complexity pruning of CART, which makes this
#'  discretization method resistant to overfitting.
#'
#' This step requires the \pkg{rpart} package. If not installed, the
#'  step will stop with a note about installing the package.
#'
#' Note that the original data will be replaced with the new bins.
#' 
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns
#' `terms` (the columns that is selected), `values` is returned.
#' 
#' @template case-weights-supervised
#'
#' @examples
#' library(modeldata)
#' data(ad_data)
#' library(rsample)
#'
#' split <- initial_split(ad_data, strata = "Class")
#'
#' ad_data_tr <- training(split)
#' ad_data_te <- testing(split)
#'
#' cart_rec <-
#'   recipe(Class ~ ., data = ad_data_tr) %>%
#'   step_discretize_cart(tau, age, p_tau, Ab_42, outcome = "Class", id = "cart splits")
#'
#' cart_rec <- prep(cart_rec, training = ad_data_tr)
#'
#' # The splits:
#' tidy(cart_rec, id = "cart splits")
#'
#' bake(cart_rec, ad_data_te, tau)
#' @seealso [embed::step_discretize_xgb()], [recipes::recipe()],
#' [recipes::prep()], [recipes::bake()]
#'
step_discretize_cart <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           outcome = NULL,
           cost_complexity = 0.01,
           tree_depth = 10,
           min_n = 20,
           rules = NULL,
           skip = FALSE,
           id = rand_id("discretize_cart")) {
    recipes::recipes_pkg_check(required_pkgs.step_discretize_cart())

    if (is.null(outcome)) {
      rlang::abort("`outcome` should select at least one column.")
    }

    add_step(
      recipe,
      step_discretize_cart_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        outcome = outcome,
        cost_complexity = cost_complexity,
        tree_depth = tree_depth,
        min_n = min_n,
        rules = rules,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }

step_discretize_cart_new <-
  function(terms, role, trained, outcome, cost_complexity, tree_depth,
           min_n, rules, skip, id, case_weights) {
    step(
      subclass = "discretize_cart",
      terms = terms,
      role = role,
      trained = trained,
      outcome = outcome,
      cost_complexity = cost_complexity,
      tree_depth = tree_depth,
      min_n = min_n,
      rules = rules,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }


cart_binning <- function(predictor, term, outcome, cost_complexity, tree_depth, 
                         min_n, wts = NULL) {
  df <- data.frame(y = outcome, x = predictor)
  if (is.null(wts)) {
    wts <- rep(1, nrow(df))
  }
  cart_mdl <-
    try(
      rpart::rpart(
        y ~ x,
        data = df,
        weights = as.double(wts),
        cp  = cost_complexity,
        minsplit = min_n,
        maxdepth = tree_depth,
        maxcompete = 0,
        maxsurrogate = 0
      ),
      silent = TRUE
    )

  if (inherits(cart_mdl, "try-error")) {
    err <- conditionMessage(attr(cart_mdl, "condition"))
    msg <-
      glue(
        "`step_discretize_cart()` failed to create a tree with error for ",
        "predictor '{term}', which will not be binned. The error: {err}"
      )
    rlang::warn(msg)
    return(numeric(0))
  }

  if (any(names(cart_mdl) == "splits")) {
    cart_split <- sort(unique(cart_mdl$splits[, "index"]))
  } else {
    msg <-
      glue(
        "`step_discretize_cart()` failed to find any meaningful splits for ",
        "predictor '{term}', which will not be binned."
      )
    rlang::warn(msg)
    cart_split <- numeric(0)
  }
  cart_split
}

#' @export
prep.step_discretize_cart <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)

  wts <- recipes::get_case_weights(info, training)
  were_weights_used <- recipes::are_weights_used(wts)
  if (isFALSE(were_weights_used)) {
    wts <- rep(1, nrow(training))
  }
  
  if (length(col_names) > 0) {
    check_type(training[, col_names])

    y_name <- recipes::recipes_eval_select(x$outcome, training, info)

    col_names <- col_names[col_names != y_name]

    rules <-
      purrr::map2(
        training[, col_names],
        col_names,
        cart_binning,
        outcome = training[[y_name]],
        cost_complexity = x$cost_complexity,
        tree_depth = x$tree_depth,
        min_n = x$min_n,
        wts = wts
      )

    has_splits <- purrr::map_lgl(rules, ~ length(.x) > 0)

    rules <- rules[has_splits]
    col_names <- col_names[has_splits]
    if (length(col_names) > 0) {
      names(rules) <- col_names
    }
  } else {
    rules <- list()
  }


  step_discretize_cart_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outcome = x$outcome,
    cost_complexity = x$cost_complexity,
    tree_depth = x$tree_depth,
    min_n = x$min_n,
    rules = rules,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_discretize_cart <- function(object, new_data, ...) {
  vars <- object$rules

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
print.step_discretize_cart <- function(x, width = max(20, options()$width - 30), ...) {
  title <- "Discretizing variables using CART "
  print_step(names(x$rules), x$terms, x$trained, title, width,
             case_weights = x$case_weights)
  invisible(x)
}

#' @rdname tidy.recipe
#' @param x A `step_discretize_cart` object.
#' @export
tidy.step_discretize_cart <- function(x, ...) {
  if (recipes::is_trained(x)) {
    num_splits <- purrr::map_int(x$rules, length)

    res <- tibble(
      terms = rep(names(x$rules), num_splits),
      values = unlist(x$rules, use.names = FALSE)
    )
  } else {
    term_names <- recipes::sel2char(x$terms)
    res <- tibble(
      variable = term_names,
      values = rlang::na_chr
    )
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.embed
#' @export
required_pkgs.step_discretize_cart <- function(x, ...) {
  c("rpart", "embed")
}
