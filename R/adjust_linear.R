#' Adjust variables using a linear model
#'
#' `step_adjust_linear()` creates a *specification* of a recipe step that will
#' adjust a variable or group of variables by linearly "residualizing out" other
#' variable(s).
#'
#' @inheritParams recipes::step_pca
#' @param role Not used by this step since no new variables are created.
#' @param remove_vars One or more selector functions to choose variables to
#'   residualize out. The predicted term-values for these variables are
#'   subtracted from the outcome (`...`).
#' @param keep_vars One or more selector functions to choose variables to
#'   _not_ residualize out.
#' @param models The [stats::lm()] object is stored here once this preprocessing
#'   step has be trained by [recipes::prep()].
#' @param drop When [recipes::bake()] is called, should the `remove_vars`
#'   variables be removed (`"remove"`; default), also the `keep_vars` variables
#'   (`"both"`) or should no variables be removed (all kept; `"none"`).
#' @template step-return
#' @details
#'
#' For each selected variable, `step_adjust_linear()` fit a _linear_ model:
#'
#' ```R
#' lm(variable ~ remove_vars + keep_vars)
#' ```
#'
#' And then adjusts `variable` but subtracting from the it sum of the predicted
#' term-wise values of `remove_vars` (using
#' [`stats::predict.lm(term = remove_vars)`][stats::predict.lm]. This is similar
#' to the functionality provided by [limma::removeBatchEffect()], and is
#' particularly useful for "removing" unwanted batch effects from
#' log-gen-expression outcomes associated with technical variables (possibly
#' without removing experimental design or grouping variables), but can be
#' applied to any situation where variables need to be adjusted as part of
#' pre-processing.
#'
#' (Prior to model fitting, numerical `remove_vars` / `keep_vars` are centered
#' and factors are effect-encoded using [stats::contr.sum()].)
#'
#' Note that the original data will be replaced with the adjusted data, possibly
#' dropping the `remove_vars` / `keep_vars` (depending on the value of the `drop` argument).
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe] this step, a tibble is returned with
#' columns `variables`, `term`, `type`, `value`, and `id`:
#'
#' \describe{
#'   \item{variables}{character, the selectors or variables selected}
#'   \item{term}{character, the variables to remove or keep / coefficient label}
#'   \item{type}{character, either "remove" or "keep"}
#'   \item{value}{numeric, the coefficient value for the term}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-supervised
#'
#' @examplesIf rlang::is_installed(c("modeldata", "ggplot2 (>= 4.0.0)"))
#'
#' library(ggplot2)
#'
#' data("penguins", package = "modeldata")
#' penguins <- na.omit(penguins)
#'
#' p <- ggplot(penguins, aes(flipper_length_mm, body_mass_g, color = sex)) +
#'   geom_point(aes(shape = species)) +
#'   stat_ellipse() +
#'   labs(title = "No adjustment")
#'
#' p
#'
#'
#' recipe <- recipe(body_mass_g ~ ., data = penguins) |>
#'   step_adjust_linear(
#'     flipper_length_mm,
#'     body_mass_g,
#'     remove_vars = vars(species),
#'     keep_vars = vars(sex),
#'     drop = "none" # keep all variables in the baked data
#'   )
#'
#' p +
#'   (prep(recipe) |>
#'     bake(new_data = penguins)) +
#'   labs(title = "Adjustment for species")
#'
#'
#' @export
step_adjust_linear <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  remove_vars = NULL,
  keep_vars = NULL,
  models = NULL,
  drop = c("remove", "both", "none"),
  skip = FALSE,
  id = rand_id("adjust_linear")
) {
  add_step(
    recipe,
    step_adjust_linear_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      remove_vars = remove_vars,
      keep_vars = keep_vars,
      models = models,
      drop = drop,
      skip = skip,
      id = id,
      case_weights = NULL
    )
  )
}

step_adjust_linear_new <- function(
  terms,
  role,
  trained,
  remove_vars,
  keep_vars,
  models,
  drop,
  skip,
  id,
  case_weights
) {
  step(
    subclass = "adjust_linear",
    terms = terms,
    role = role,
    trained = trained,
    remove_vars = remove_vars,
    keep_vars = keep_vars,
    models = models,
    drop = drop,
    skip = skip,
    id = id,
    case_weights = case_weights
  )
}

#' @export
prep.step_adjust_linear <- function(x, training, info = NULL, ...) {
  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts)
  if (isFALSE(were_weights_used)) {
    wts <- rep(1, nrow(training))
  }

  col_names <- recipes_eval_select(x$terms, training, info)

  if (is.null(x$remove_vars)) {
    cli::cli_abort(
      c(
        "The `remove_vars` argument must be specified.",
        "i" = "This is the variable(s) you want to remove the effect of."
      )
    )
  }

  remove_names <- recipes_eval_select(x$remove_vars, training, info)

  # Identify Preserved columns (Design)
  # Handle case where keep_vars is NULL
  if (!is.null(x$keep_vars)) {
    keep_names <- recipes_eval_select(x$keep_vars, training, info)

    if (any(keep_names %in% remove_names)) {
      cli::cli_abort(
        c(
          "The `keep_vars` and `remove_vars` selectors must be disjoint.",
          "x" = "The following variables are in both: {intersect(keep_names, remove_names)}"
        )
      )
    }
  } else {
    keep_names <- NULL
  }

  .contrasts <- NULL
  all_names <- c(remove_names, keep_names)
  is_fct <- purrr::map_lgl(all_names, \(v) is.factor(training[[v]]))
  is_num <- purrr::map_lgl(all_names, \(v) is.numeric(training[[v]]))
  other_names <- all_names[!(is_fct | is_num)]

  if (length(other_names) > 0L) {
    cli::cli_abort(
      c(
        "The `remove_vars` and `keep_vars` selectors must be either factors or numeric.",
        "x" = "The following variable is neither: {other_names}"
      )
    )
  }

  if (any(is_num)) {
    all_names[is_num] <- sprintf("scale(%s, scale = FALSE)", all_names[is_num])
  }

  if (any(is_fct)) {
    .contrasts <- stats::setNames(
      rep(list("contr.sum"), sum(is_fct)),
      all_names[is_fct]
    )
  }

  model_list <- list()

  for (col in col_names) {
    # Create formula: Target ~ Remove1 + Keep1 + ...
    # We combine both sets of variables for the fit
    ff <- stats::reformulate(
      response = col,
      termlabels = all_names
    )

    # Fit and store the model
    model_list[[col]] <- stats::lm(
      ff,
      data = training,
      weights = wts,
      contrasts = .contrasts
    )
  }

  drop <- match.arg(x$drop, choices = c("remove", "both", "none"))

  step_adjust_linear_new(
    terms = col_names,
    role = x$role,
    trained = TRUE,
    remove_vars = remove_names,
    keep_vars = keep_names,
    models = model_list,
    drop = drop,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_adjust_linear <- function(object, new_data, ...) {
  remove_names <- object$remove_vars
  keep_names <- object$keep_vars

  check_new_data(
    unique(c(names(object$models), remove_names, keep_names)),
    object,
    new_data
  )

  for (col in names(object$models)) {
    model <- object$models[[col]]

    # Crucial Step: use type = "terms"
    # This returns a matrix with one column per independent variable,
    # representing that variable's contribution to the prediction.
    # It handles factors (dummification) automatically.
    term_preds <- stats::predict(model, newdata = new_data, type = "terms")

    # Identify which columns in the term matrix correspond to our `remove_vars`
    # Note: `predict` names columns by the variable name.
    cols_to_subtract <-
      gsub("^scale\\((.*), scale = FALSE\\)$", "\\1", colnames(term_preds)) %in%
      remove_names

    if (any(cols_to_subtract)) {
      # Sum the effects of the nuisance variables
      nuisance_effect <- rowSums(term_preds[, cols_to_subtract, drop = FALSE])

      # Subtract nuisance effect from original data
      # Result = (Signal + Batch + Noise) - (Batch) = Signal + Noise
      new_data[[col]] <- new_data[[col]] - nuisance_effect
    }
  }

  if (object$drop == "remove") {
    new_data <- new_data[,
      !(colnames(new_data) %in% remove_names),
      drop = FALSE
    ]
  } else if (object$drop == "both") {
    new_data <- new_data[,
      !(colnames(new_data) %in% c(remove_names, keep_names)),
      drop = FALSE
    ]
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_adjust_linear <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "Linearly adjusting variables"
  print_step(
    names(x$models),
    x$terms,
    x$trained,
    title,
    width,
    case_weights = x$case_weights
  )
  invisible(x)
}

#' @rdname step_adjust_linear
#' @usage NULL
#' @export
tidy.step_adjust_linear <- function(x, ...) {
  to_chr <- function(y) {
    if (is.null(y)) {
      character(0)
    } else if (is.character(y)) {
      y
    } else {
      sel2char(y)
    }
  }

  remove_vars <- to_chr(x$remove_vars)
  keep_vars <- to_chr(x$keep_vars)

  if (is_trained(x)) {
    res <- purrr::map(x$models, \(mod) {
      a <- attr(stats::model.matrix(mod), "assign")
      a[a == 0] <- NA
      trm <- attr(stats::terms(mod), "term.labels")
      trm <- gsub("^scale\\((.*), scale = FALSE\\)$", "\\1", trm)
      b <- stats::coef(mod)
      tibble(term = names(b), type = trm[a], value = b)
    }) |>
      dplyr::bind_rows(.id = "variables")
  } else {
    term_names <- to_chr(x$terms)
    res <- as_tibble(
      expand.grid(
        variables = term_names,
        term = c(remove_vars, keep_vars),
        stringsAsFactors = FALSE
      )
    )
    res$type <- res$term
    res$value <- NA_real_
  }

  res$type[res$type %in% remove_vars] <- "remove"
  res$type[res$type %in% keep_vars] <- "keep"
  res <- res[order(res$variables), ]
  res$id <- x$id
  res
}

#' @rdname required_pkgs.embed
#' @export
required_pkgs.step_adjust_linear <- function(x, ...) {
  c("embed")
}
