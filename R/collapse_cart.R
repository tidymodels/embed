#' Supervised Collapsing of Factor Levels
#'
#' `step_collapse_cart()` creates a *specification* of a recipe step that can
#' collapse factor levels into a smaller set using a supervised tree.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [recipes::selections] for more details. For the `tidy`
#'   method, these are not currently used.
#' @param outcome A call to `vars` to specify which variable is used as the
#'   outcome to train CART models in order to pool factor levels.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated.
#' @param cost_complexity A non-negative value that regulates the complexity of
#'   the tree when pruning occurs. Values near 0.1 usually correspond to a tree
#'   with a single splits. Values of zero correspond to unpruned tree.
#' @param min_n An integer for how many data points are required to make further
#'   splits during the tree growing process. Larger values correspond to less
#'   complex trees.
#' @param results A list of results to convert to new factor levels.
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   [recipes::bake]? While all operations are baked when [recipes::prep] is run, some
#'   operations may not be able to be conducted on new data (e.g. processing the
#'   outcome variable(s)). Care should be taken when using `skip = TRUE` as it
#'   may affect the computations for subsequent operations
#' @param id A character string that is unique to this step to identify it.
#' @return An updated recipe step.
#' @details
#'
#' This step uses a CART tree (classification or regression) to group the
#' existing factor levels into a potentially smaller set. It changes the levels
#' in the factor predictor (and the `tidy()` method can be used to understand
#' the translation).
#'
#' There are a few different ways that the step will not be able to collapse
#' levels. If the model fails or, if the results have each level being in its
#' own split, the original factor levels are retained. There are also cases
#' where there is "no admissible split" which means that the model could not
#' find any signal in the data.
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe] this step, a tibble is returned with
#' columns `terms`, `old`, `new`, and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{old}{character, the old levels}
#'   \item{new}{character, the new levels}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed(c("modeldata", "rpart"))
#' data(ames, package = "modeldata")
#' ames$Sale_Price <- log10(ames$Sale_Price)
#'
#' rec <-
#'   recipe(Sale_Price ~ ., data = ames) |>
#'   step_collapse_cart(
#'     Sale_Type, Garage_Type, Neighborhood,
#'     outcome = vars(Sale_Price)
#'   ) |>
#'   prep()
#' tidy(rec, number = 1)
#' @export
step_collapse_cart <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    outcome = NULL,
    cost_complexity = 0.0001,
    min_n = 5,
    results = NULL,
    skip = FALSE,
    id = rand_id("step_collapse_cart")
  ) {
    recipes_pkg_check(required_pkgs.step_discretize_cart())

    check_number_decimal(cost_complexity, min = 0)
    check_number_whole(min_n, min = 1)

    add_step(
      recipe,
      step_collapse_cart_new(
        terms = rlang::enquos(...),
        trained = trained,
        role = role,
        outcome = outcome,
        cost_complexity = cost_complexity,
        min_n = min_n,
        results = results,
        skip = skip,
        id = id
      )
    )
  }
step_collapse_cart_new <-
  function(
    terms,
    role,
    trained,
    outcome,
    cost_complexity,
    min_n,
    results,
    skip,
    id
  ) {
    step(
      subclass = "collapse_cart",
      terms = terms,
      role = role,
      trained = trained,
      outcome = outcome,
      cost_complexity = cost_complexity,
      min_n = min_n,
      results = results,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_collapse_cart <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  y_name <- recipes_eval_select(x$outcome, training, info)
  check_type(training[, col_names], types = c("string", "factor", "ordered"))

  if (length(col_names) > 0) {
    keys <- purrr::map2(
      training[, col_names],
      col_names,
      collapse_rpart,
      y = training[[y_name]],
      cp = x$cost_complexity,
      minsplit = x$min_n
    )
    check_res <- purrr::map_lgl(keys, tibble::is_tibble)
    keys <- keys[check_res]
  } else {
    keys <- list()
  }

  step_collapse_cart_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outcome = y_name,
    cost_complexity = x$cost_complexity,
    min_n = x$min_n,
    results = keys,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_collapse_cart <- function(object, new_data, ...) {
  col_names <- names(object$results)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    new_data <- convert_keys(col_name, object$results[[col_name]], new_data)
  }

  new_data
}

#' @export
print.step_collapse_cart <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Collapsing factor levels using CART "
    print_step(names(x$results), x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname step_collapse_cart
#' @usage NULL
#' @export
tidy.step_collapse_cart <- function(x, ...) {
  if (is_trained(x)) {
    if (length(x$results) == 0) {
      res <- tibble(terms = character(), value = double())
    } else {
      res <- purrr::map2_dfr(x$results, names(x$results), format_collapse_keys)
    }
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = na_dbl
    )
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.embed
#' @export
required_pkgs.step_collapse_cart <- function(x, ...) {
  c("rpart", "embed")
}

# ------------------------------------------------------------------------------

collapse_rpart <- function(x, feature_name, y, prefix = feature_name, ...) {
  # check that x is factor
  dat <- data.frame(x = x, y = y)
  split_model <- try(rpart::rpart(y ~ x, data = dat, ...), silent = TRUE)
  if (inherits(split_model, "try-error")) {
    return(list())
  }

  # check for no admissible split
  if (!any(names(split_model) == "splits")) {
    return(list())
  }

  term_nodes <- split_model$frame[split_model$frame["var"] == "<leaf>", ]
  term_nodes_ind <- as.numeric(rownames(term_nodes))

  lvls <- attr(split_model, "xlevels")[[1]]

  groups <-
    purrr::map(
      term_nodes_ind,
      ~ rpart::path.rpart(split_model, .x, print.it = FALSE)[[1]]
    ) |>
    purrr::map(~ .x[length(.x)]) |>
    purrr::map(~ strsplit(.x, "=")[[1]]) |>
    purrr::map(~ .x[length(.x)]) |>
    purrr::map(~ strsplit(.x, ",")[[1]])
  group_size <- purrr::map_int(groups, length)

  # check for each level a group
  if (length(group_size) == length(lvls)) {
    return(list())
  }

  key <-
    tibble::tibble(
      var = unlist(groups),
      group = rep(seq_along(group_size), group_size)
    ) |>
    dplyr::mutate(
      var = factor(var, levels = lvls),
      group_f = gsub(" ", "0", format(group)),
      .group = paste0(prefix, "_", group_f),
      .group = factor(.group)
    ) |>
    dplyr::select(dplyr::all_of(c("var", ".group"))) |>
    setNames(c(feature_name, ".group"))
  key
}

format_collapse_keys <- function(x, nm) {
  setNames(x, c("old", "new")) |>
    dplyr::mutate(
      dplyr::across(where(is.factor), as.character),
      terms = nm
    ) |>
    dplyr::relocate(terms)
}

convert_keys <- function(nm, keys, dat) {
  rn <- list(".group")
  names(rn) <- nm
  col_nms <- names(dat)
  dat <-
    dat |>
    dplyr::mutate(.rows = dplyr::row_number()) |>
    dplyr::left_join(keys, by = nm) |>
    dplyr::select(-dplyr::all_of(nm)) |>
    dplyr::rename(!!!rn) |>
    dplyr::arrange(.rows)

  dat[, col_nms]
}
