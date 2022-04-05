#' Supervised Factor Conversions into Linear Functions using Bayesian Likelihood Encodings
#'
#' `step_lencode_bayes` creates a *specification* of a recipe step that
#'  will convert a nominal (i.e. factor) predictor into a single set of
#'  scores derived from a generalized linear model estimated using
#'  Bayesian analysis.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_lencode_bayes`, this indicates the variables to be encoded
#'  into a numeric format. See [recipes::selections()] for more details. For
#'  the `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param outcome A call to `vars` to specify which variable is
#'  used as the outcome in the generalized linear model. Only
#'  numeric and two-level factors are currently supported.
#' @param options A list of options to pass to [rstanarm::stan_glmer()].
#' @param verbose A logical to control the default printing by
#'  [rstanarm::stan_glmer()].
#' @param mapping A list of tibble results that define the
#'  encoding. This is `NULL` until the step is trained by
#'  [recipes::prep.recipe()].
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [recipes::bake.recipe()]? While all operations are baked
#'  when [recipes::prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @param id A character string that is unique to this step to identify it.
#' @return An updated version of `recipe` with the new step added
#'  to the sequence of existing steps (if any). For the `tidy`
#'  method, a tibble with columns `terms` (the selectors or
#'  variables for encoding), `level` (the factor levels), and
#'  `value` (the encodings).
#' @keywords datagen
#' @concept preprocessing encoding
#' @export
#' @details For each factor predictor, a generalized linear model
#'  is fit to the outcome and the coefficients are returned as the
#'  encoding. These coefficients are on the linear predictor scale
#'  so, for factor outcomes, they are in log-odds units. The
#'  coefficients are created using a no intercept model and, when
#'  two factor outcomes are used, the log-odds reflect the event of
#'  interest being the _first_ level of the factor.
#'
#' For novel levels, a slightly timmed average of the coefficients
#'  is returned.
#'
#' A hierarchical generalized linear model is fit using
#'  [rstanarm::stan_glmer()] and no intercept via
#'
#' ```
#'   stan_glmer(outcome ~ (1 | predictor), data = data, ...)
#' ```
#'
#' where the `...` include the `family` argument (automatically
#'  set by the step) as well as any arguments given to the `options`
#'  argument to the step. Relevant options include `chains`, `iter`,
#'  `cores`, and arguments for the priors (see the links in the
#'  References below). `prior_intercept` is the argument that has the
#'  most effect on the amount of shrinkage.
#'
#'
#' @references
#' Micci-Barreca D (2001) "A preprocessing scheme for
#'  high-cardinality categorical attributes in classification and
#'  prediction problems," ACM SIGKDD Explorations Newsletter, 3(1),
#'  27-32.
#'
#' Zumel N and Mount J (2017) "vtreat: a data.frame Processor for
#'  Predictive Modeling," arXiv:1611.09477
#'
#' "Hierarchical Partial Pooling for Repeated Binary Trials"
#'  \url{https://tinyurl.com/stan-pooling}
#'
#' "Prior Distributions for `rstanarm`` Models"
#'  \url{https://tinyurl.com/stan-priors}
#'
#' "Estimating Generalized (Non-)Linear Models with Group-Specific
#' Terms with `rstanarm`" \url{https://tinyurl.com/stan-glm-grouped}
#'
#' @examples
#' library(recipes)
#' library(dplyr)
#' library(modeldata)
#'
#' data(grants)
#'
#' set.seed(1)
#' grants_other <- sample_n(grants_other, 500)
#' \donttest{
#' reencoded <- recipe(class ~ sponsor_code, data = grants_other) %>%
#'   step_lencode_bayes(sponsor_code, outcome = vars(class))
#' }
step_lencode_bayes <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           outcome = NULL,
           options = list(seed = sample.int(10^5, 1)),
           verbose = FALSE,
           mapping = NULL,
           skip = FALSE,
           id = rand_id("lencode_bayes")) {
    if (is.null(outcome)) {
      rlang::abort("Please list a variable in `outcome`")
    }
    add_step(
      recipe,
      step_lencode_bayes_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        outcome = outcome,
        options = options,
        verbose = verbose,
        mapping = mapping,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }

step_lencode_bayes_new <-
  function(terms, role, trained, outcome, options, verbose, mapping, skip, id,
           case_weights) {
    step(
      subclass = "lencode_bayes",
      terms = terms,
      role = role,
      trained = trained,
      outcome = outcome,
      options = options,
      verbose = verbose,
      mapping = mapping,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }


#' @export
prep.step_lencode_bayes <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  
  wts <- recipes::get_case_weights(info, training)
  were_weights_used <- recipes::are_weights_used(wts)
  if (isFALSE(were_weights_used) || is.null(wts)) {
    wts <- NULL
  }
  
  if (length(col_names) > 0) {
    check_type(training[, col_names], quant = FALSE)
    y_name <- recipes::recipes_eval_select(x$outcome, training, info)
    res <-
      map(training[, col_names], stan_coefs,
        y = training[, y_name],
        x$options, x$verbose, wts
      )
  } else {
    res <- list()
  }

  step_lencode_bayes_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outcome = x$outcome,
    options = x$options,
    verbose = x$verbose,
    mapping = res,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

stan_coefs <- function(x, y, options, verbose, wts = NULL, ...) {
  rlang::check_installed("rstanarm")
  if (is.factor(y[[1]])) {
    fam <- binomial()
  } else {
    fam <- gaussian()
  }
  form <- as.formula(paste0(names(y), "~ (1|value)"))

  if (is.vector(x) | is.factor(x)) {
    x <- tibble(value = x)
  } else {
    x <- as_tibble(x)
  }

  args <-
    list(
      form,
      data = bind_cols(x, y),
      family = fam,
      na.action = na.omit
    )
  if (length(options) > 0) {
    args <- c(args, options)
  }
  if (!is.null(wts)) {
    args$weights <- as.numeric(wts)
  }

  cl <- rlang::call2("stan_glmer", .ns = "rstanarm", !!!args)

  if (!verbose) {
    junk <- capture.output(mod <- rlang::eval_tidy(cl))
  } else {
    mod <- rlang::eval_tidy(cl)
  }

  coefs <- coef(mod)$value
  coefs <- as.data.frame(coefs)
  coefs <- set_names(coefs, "..value")
  coefs <- rownames_to_column(coefs, "..level")
  coefs <- as_tibble(coefs)
  mean_coef <- mean(coefs$..value, na.rm = TRUE, trim = .1)
  coefs$..value[is.na(coefs$..value)] <- mean_coef
  new_row <- tibble(..level = "..new", ..value = mean_coef)
  coefs <- bind_rows(coefs, new_row)
  if (is.factor(y[[1]])) {
    coefs$..value <- -coefs$..value
  }
  coefs
}

#' @export
bake.step_lencode_bayes <- function(object, new_data, ...) {
  for (col in names(object$mapping)) {
    new_data[, col] <- map_glm_coef(new_data[, col], object$mapping[[col]])
  }

  new_data
}


#' @export
print.step_lencode_bayes <-
  function(x, width = max(20, options()$width - 31), ...) {
    title <- "Linear embedding for factors via Bayesian GLM for "
    print_step(names(x$mapping), x$terms, x$trained, title, width,
               case_weights = x$case_weights)
    invisible(x)
  }


#' @rdname step_lencode_bayes
#' @param x A `step_lencode_bayes` object.
#' @export
#' @export tidy.step_lencode_bayes
tidy.step_lencode_bayes <- function(x, ...) {
  if (is_trained(x)) {
    for (i in seq_along(x$mapping)) {
      x$mapping[[i]]$terms <- names(x$mapping)[i]
    }
    res <- bind_rows(x$mapping)
    names(res) <- gsub("^\\.\\.", "", names(res))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      level = rep(na_chr, length(term_names)),
      value = rep(na_dbl, length(term_names)),
      terms = term_names
    )
  }
  res$id <- x$id
  res
}

#' S3 methods for tracking which additional packages are needed for steps.
#'
#' Recipe-adjacent packages always list themselves as a required package so that
#' the steps can function properly within parallel processing schemes.
#' @param x A recipe step
#' @return A character vector
#' @rdname required_pkgs.embed
#' @keywords internal
#' @export
required_pkgs.step_lencode_bayes <- function(x, ...) {
  c("rstanarm", "embed")
}
