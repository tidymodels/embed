#' Supervised Factor Conversions into Linear Functions using Bayesian Likelihood Encodings
#'
#' `step_lencode_mixed` creates a *specification* of a recipe step that
#'  will convert a nominal (i.e. factor) predictor into a single set of
#'  scores derived from a generalized linear mixed model.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_lencode_mixed`, this indicates the variables to be encoded
#'  into a numeric format. See [recipes::selections()] for more details. For
#'  the `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param outcome A call to `vars` to specify which variable is
#'  used as the outcome in the generalized linear model. Only
#'  numeric and two-level factors are currently supported.
#' @param options A list of options to pass to [lme4::lmer()] or
#'  [lme4::glmer()].
#' @param mapping A list of tibble results that define the
#'  encoding. This is `NULL` until the step is trained by
#'  [recipes::prep()].
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [recipes::bake()]? While all operations are baked
#'  when [recipes::prep()] is run, some operations may not be able to be
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
#'  [lme4::lmer()] or [lme4::glmer()], depending
#'  on the nature of the outcome, and no intercept via
#'
#' ```
#'   lmer(outcome ~ 1 + (1 | predictor), data = data, ...)
#' ```
#'
#' where the `...` include the `family` argument (automatically
#'  set by the step) as well as any arguments given to the `options`
#'  argument to the step. Relevant options include `control` and
#'  others.
#'  
#' # Tidying
#' 
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns
#' `terms` (the selectors or variables selected), `value` and `component` is
#' returned.
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
#'   step_lencode_mixed(sponsor_code, outcome = vars(class))
#' }
step_lencode_mixed <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           outcome = NULL,
           options = list(verbose = 0),
           mapping = NULL,
           skip = FALSE,
           id = rand_id("lencode_mixed")) {
    if (is.null(outcome)) {
      rlang::abort("Please list a variable in `outcome`")
    }
    add_step(
      recipe,
      step_lencode_mixed_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        outcome = outcome,
        options = options,
        mapping = mapping,
        skip = skip,
        id = id
      )
    )
  }

step_lencode_mixed_new <-
  function(terms, role, trained, outcome, options, mapping, skip, id) {
    step(
      subclass = "lencode_mixed",
      terms = terms,
      role = role,
      trained = trained,
      outcome = outcome,
      options = options,
      mapping = mapping,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_lencode_mixed <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  if (length(col_names) > 0) {
    check_type(training[, col_names], quant = FALSE)
    y_name <- recipes::recipes_eval_select(x$outcome, training, info)
    if (is.factor(training[[y_name]])) {
      if (length(levels(training[[y_name]])) > 2) {
        rlang::abort(glue(
          "Mixed effects methods here are only implemented for ",
          "two-class problems."
        ))
      }
    }
    res <-
      map(training[, col_names], lme_coefs,
        y = training[[y_name]],
        x$options
      )
  } else {
    res <- list()
  }
  step_lencode_mixed_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outcome = x$outcome,
    options = x$options,
    mapping = res,
    skip = x$skip,
    id = x$id
  )
}


lme_coefs <- function(x, y, ...) {
  rlang::check_installed("lme4")
  args <- list(
    formula = y ~ 1 + (1 | value),
    data = data.frame(value = x, y = y),
    na.action = na.omit
  )

  dots <- list(...)
  if (length(dots) > 0) {
    args <- c(args, dots[[1]])
  }

  if (!is.factor(y[[1]])) {
    cl <- rlang::call2("lmer", .ns = "lme4", !!!args)
    mod <- rlang::eval_tidy(cl)
  } else {
    args$data$y <- as.numeric(args$data$y) - 1
    args$family <- stats::binomial
    cl <- rlang::call2("glmer", .ns = "lme4", !!!args)
    mod <- rlang::eval_tidy(cl)
  }

  coefs <- coef(mod)$value
  ..levels <- rownames(coefs)
  coefs <- coefs[, 1]
  names(coefs) <- ..levels
  mean_coef <- mean(coefs, na.rm = TRUE, trim = .1)
  coefs[is.na(coefs)] <- mean_coef
  coefs <- c(coefs, ..new = mean_coef)
  if (is.factor(y[[1]])) {
    coefs <- -coefs
  }
  tibble(
    ..level = names(coefs),
    ..value = unname(coefs)
  )
}


map_lme_coef <- function(dat, mapping) {
  new_val <- mapping$..value[mapping$..level == "..new"]
  dat <- dat %>%
    mutate(..order = 1:nrow(dat)) %>%
    set_names(c("..level", "..order")) %>%
    mutate(..level = as.character(..level))
  mapping <- mapping %>% dplyr::filter(..level != "..new")
  dat <- left_join(dat, mapping, by = "..level") %>%
    arrange(..order)
  dat$..value[is.na(dat$..value)] <- new_val
  dat$..value
}


#' @export
bake.step_lencode_mixed <- function(object, new_data, ...) {
  for (col in names(object$mapping)) {
    new_data[, col] <- map_lme_coef(new_data[, col], object$mapping[[col]])
  }

  new_data
}

#' @export
print.step_lencode_mixed <-
  function(x, width = max(20, options()$width - 31), ...) {
    title <- "Linear embedding for factors via mixed effects for "
    print_step(names(x$mapping), x$terms, x$trained, title, width)
    invisible(x)
  }


#' @rdname step_lencode_mixed
#' @param x A `step_lencode_mixed` object.
#' @export
#' @export tidy.step_lencode_mixed
tidy.step_lencode_mixed <- function(x, ...) {
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


#' @rdname required_pkgs.embed
#' @export
required_pkgs.step_lencode_mixed <- function(x, ...) {
  c("lme4", "embed")
}
