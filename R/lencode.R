#' Likelihood encoding using analytical formula
#'
#' `step_lencode()` creates a *specification* of a recipe step that will convert
#' a nominal (i.e. factor) predictor into a single set of scores derived
#' analytically.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose variables. For
#'   `step_lencode()`, this indicates the variables to be encoded into a
#'   numeric format. See [recipes::selections()] for more details. For the
#'   `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are created.
#' @param outcome A call to `vars` to specify which variable is used as the
#'   outcome. Only numeric and two-level factors are currently supported.
#' @param smooth A logical, default to `TRUE`, should the estimates of groups
#'   with low counts be pulled towards the gobal estimate? Defaults to `TRUE`.
#'   See Details for how this is done. This is also known as partial pooling or
#'   shrinkage. Only works for numeric outcomes.
#' @param mapping A list of tibble results that define the encoding. This is
#'   `NULL` until the step is trained by [recipes::prep()].
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   [recipes::bake()]? While all operations are baked when [recipes::prep()] is
#'   run, some operations may not be able to be conducted on new data (e.g.
#'   processing the outcome variable(s)). Care should be taken when using `skip
#'   = TRUE` as it may affect the computations for subsequent operations
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated.
#' @param id A character string that is unique to this step to identify it.
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of existing steps (if any). For the `tidy` method, a tibble with
#'   columns `terms` (the selectors or variables for encoding), `level` (the
#'   factor levels), and `value` (the encodings).
#' @keywords datagen
#' @concept preprocessing encoding
#' @details
#'
#' Each selected nominal predictor will be replaced by a numeric predictor.
#' Each unique value of the nominal predictor is replaced by a numeric value.
#' Thse values are calculated differently depending on the type of the outcome.
#'
#' For **numeric** outcomes each value is the average value of the outcome
#' inside each of the levels of the predictor. Unseen levels of the predictor
#' will be using the global mean of the predictor.
#' If case weights are used then a weighted mean is calculated instead.
#'
#' For **nominal** outcomes each value is the log odds of the of the first level
#' of the outcome variable being present, within each level of the levels of the
#' predictor. Unseen levels will be replaced by the global log odds without
#' stratification.
#' If case weights are used then a weighted log odds is calculated.
#'
#' If no or all occurances happens then the log odds is calculated using
#' `p = (2 * nrow(data) - 1) / (2 * nrow(data))` to avoid infinity that would
#' happen by taking the log of `0`.
#'
#' For numeric outcomes where `smooth = TRUE`, the following adjustment is done.
#'
#' \deqn{
#' estimate = (n / global_{var}) /
#' (n / global_{var} + 1 / outcome_{var}) *
#'  estimate +
#'  (1 / outcome_{var}) / (n / global_{var} + 1 / outcome_{var}) * global_{mean}
#' }
#'
#' Where \eqn{n} is the number of observations in the group.
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe] this step, a tibble is returned
#' with columns `level`, `value`, `terms`, and `id`:
#'
#' \describe{
#'   \item{level}{character, the factor levels}
#'   \item{value}{numeric, the encoding}
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-supervised
#'
#' @references
#'
#' Micci-Barreca D (2001) "A preprocessing scheme for high-cardinality
#' categorical attributes in classification and prediction problems," ACM SIGKDD
#' Explorations Newsletter, 3(1), 27-32.
#'
#' Zumel N and Mount J (2017) "vtreat: a data.frame Processor for Predictive
#' Modeling," arXiv:1611.09477
#'
#' @examplesIf rlang::is_installed("modeldata")
#' library(recipes)
#' library(dplyr)
#' library(modeldata)
#'
#' data(grants)
#'
#' set.seed(1)
#' grants_other <- sample_n(grants_other, 500)
#' reencoded <- recipe(class ~ sponsor_code, data = grants_other) |>
#'   step_lencode(sponsor_code, outcome = vars(class), smooth = FALSE) |>
#'   prep()
#'
#' bake(reencoded, grants_other)
#'
#' tidy(reencoded, 1)
#' @seealso [step_lencode_bayes()], [step_lencode_glm()], [step_lencode_mixed()]
#' @export
step_lencode <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    outcome = NULL,
    smooth = TRUE,
    mapping = NULL,
    skip = FALSE,
    id = rand_id("lencode")
  ) {
    if (is.null(outcome)) {
      cli::cli_abort("Please list a variable in {.arg outcome}.")
    }
    add_step(
      recipe,
      step_lencode_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        outcome = outcome,
        smooth = smooth,
        mapping = mapping,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }

step_lencode_new <-
  function(
    terms,
    role,
    trained,
    outcome,
    smooth,
    mapping,
    skip,
    id,
    case_weights
  ) {
    step(
      subclass = "lencode",
      terms = terms,
      role = role,
      trained = trained,
      outcome = outcome,
      smooth = smooth,
      mapping = mapping,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

#' @export
prep.step_lencode <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts)
  if (isFALSE(were_weights_used) || is.null(wts)) {
    wts <- NULL
  }

  if (length(col_names) > 0) {
    check_type(training[, col_names], types = c("string", "factor", "ordered"))
    y_name <- recipes_eval_select(x$outcome, training, info)
    res <- purrr::map(
      training[, col_names],
      lencode_calc,
      y = training[[y_name]],
      wts = wts,
      smooth = x$smooth
    )
  } else {
    res <- list()
  }
  step_lencode_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outcome = x$outcome,
    smooth = x$smooth,
    mapping = res,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}
lencode_calc <- function(x, y, wts = NULL, smooth = TRUE) {
  if (!is.numeric(y) && !is.factor(y) && !is.character(y)) {
    cli::cli_abort(
      "Only works nominal or numeric {.arg outcome}, 
      not {.obj_type_friendly {y}}."
    )
  }

  data <- tibble::new_tibble(
    list(..level = x, ..value = y, wts = wts)
  )

  if (is.numeric(y)) {
    fns <- list("weighted" = stats::weighted.mean, "unweighted" = mean)
  }
  if (is.factor(y) || is.character(y)) {
    fns <- list("weighted" = weighted_log_odds, "unweighted" = log_odds)
  }

  if (is.null(wts)) {
    call <- rlang::call2(fns$unweighted)
    call <- rlang::call_modify(call, quote(..value))
  } else {
    call <- rlang::call2(fns$weighted)
    call <- rlang::call_modify(call, quote(..value), quote(wts))
  }

  res <- dplyr::summarise(
    data,
    ..value := {{ call }},
    .by = ..level
  )

  unseen_value <- rlang::eval_tidy(call, data)

  if (is.factor(y) || is.character(y)) {
    res$..value <- adjust_infinities(res$..value, n = nrow(data))
  }

  if (smooth) {
    if (!is.numeric(y)) {
      cli::cli_abort("{.code smooth = TRUE} only works for numeric outcomes.")
    }

    global_var <- var(data$..value)
    global_mean <- mean(data$..value)

    counts <- dplyr::summarise(
      data,
      n = dplyr::n(),
      var_outcome = var(..value),
      .by = ..level
    )

    res <- dplyr::left_join(res, counts, by = "..level")

    res <- res |>
      dplyr::mutate(
        ..value = (n / global_var) /
          (n / global_var + 1 / var_outcome) *
          ..value +
          (1 / var_outcome) / (n / global_var + 1 / var_outcome) * global_mean
      ) |>
      dplyr::select(-n, -var_outcome)
  }

  unseen <- tibble::new_tibble(
    list(
      ..level = "..new",
      ..value = unseen_value
    )
  )

  dplyr::bind_rows(res, unseen)
}

log_odds <- function(x) {
  p <- (sum(x == levels(x)[1])) / length(x)
  log(p / (1 - p))
}

weighted_log_odds <- function(x, wts) {
  wts <- as.numeric(wts)
  p <- (sum((x == levels(x)[1]) * wts)) / sum(wts)
  log(p / (1 - p))
}

adjust_infinities <- function(x, n) {
  p <- (2 * n - 1) / (2 * n)
  log_odds <- log(p / (1 - p))

  dplyr::if_else(is.infinite(x), log_odds, x)
}

#' @export
bake.step_lencode <- function(object, new_data, ...) {
  col_names <- names(object$mapping)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    new_data[[col_name]] <- map_glm_coef(
      dat = new_data[, col_name], # map_glm_coef() expects a tibble
      mapping = object$mapping[[col_name]]
    )
  }

  new_data
}

#' @export
print.step_lencode <-
  function(x, width = max(20, options()$width - 31), ...) {
    title <- "Linear embedding for factors via GLM for "
    print_step(
      names(x$mapping),
      x$terms,
      x$trained,
      title,
      width,
      case_weights = x$case_weights
    )
    invisible(x)
  }

#' @rdname step_lencode
#' @usage NULL
#' @export
tidy.step_lencode <- function(x, ...) {
  if (is_trained(x)) {
    if (length(x$mapping) == 0) {
      res <- tibble(
        terms = character(),
        level = character(),
        value = double()
      )
    } else {
      for (i in seq_along(x$mapping)) {
        x$mapping[[i]]$terms <- names(x$mapping)[i]
      }
      res <- bind_rows(x$mapping)
      names(res) <- gsub("^\\.\\.", "", names(res))
    }
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      level = rep(na_chr, length(term_names)),
      value = rep(na_dbl, length(term_names))
    )
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.embed
#' @export
required_pkgs.step_lencode <- function(x, ...) {
  c("embed")
}
