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
#'  [recipes::prep.recipe()].
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [recipes::bake.recipe()]? While all operations are baked
#'  when [recipes::prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
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
#' 
#' data(okc)
#' 
#' reencoded <- recipe(Class ~ age + location, data = okc) %>%
#'   step_lencode_mixed(location, outcome = vars(Class))
#' 
#' # See https://topepo.github.io/embed/ for examples

#' @importFrom recipes add_step step terms_select sel2char ellipse_check
step_lencode_mixed <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           outcome = NULL,
           options = list(verbose = 0),
           mapping = NULL,
           skip = FALSE) {
    if (is.null(outcome))
      stop("Please list a variable in `outcome`", call. = FALSE)
    add_step(
      recipe,
      step_lencode_mixed_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        outcome = outcome,
        options = options,
        mapping = mapping,
        skip = skip
      )
    )
  }

step_lencode_mixed_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           outcome = NULL,
           options = NULL,
           mapping = NULL,
           skip = FALSE) {
    step(
      subclass = "lencode_mixed",
      terms = terms,
      role = role,
      trained = trained,
      outcome = outcome,
      options = options,
      mapping = mapping,
      skip = skip
    )
  }

#' @importFrom recipes check_type
#' @export
prep.step_lencode_mixed <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names], quant = FALSE)
  y_name <- terms_select(x$outcome, info = info)
  if (is.factor(training[[y_name]])) {
    if (length(levels(training[[y_name]])) > 2) {
      stop("Mixed effects methods here are on;y implemented for ",
           "two-class problems.",
           call. = FALSE)
    }
  }
  res <-
    map(training[, col_names], lme_coefs, y = training[[y_name]],
        x$options)
  x$mapping <- res
  x$trained <- TRUE
  x
}

#' @importFrom stats as.formula binomial coef gaussian na.omit
#' @importFrom dplyr bind_cols as_tibble
#' @importFrom lme4 glmer lmer
lme_coefs <- function(x, y, ...) {
  
  args <- list(
    formula = y ~ 1 + (1 | value),
    data = data.frame(value = x, y = y),
    na.action = na.omit
  )
  
  dots <- list(...)
  if (length(dots) > 0)
    args <- c(args, dots[[1]])
  
  if (!is.factor(y[[1]])) {
    mod <- do.call(lme4::lmer, args)
  } else {
    args$data$y <- as.numeric(args$data$y) - 1
    args$family <- stats::binomial
    mod <- do.call(lme4::glmer, args)
  }
  
  coefs <- coef(mod)$value
  ..levels <- rownames(coefs)
  coefs <- coefs[,1]
  names(coefs) <- ..levels
  mean_coef <- mean(coefs, na.rm = TRUE, trim = .1)
  coefs[is.na(coefs)] <- mean_coef
  coefs <- c(coefs, ..new = mean_coef)
  if(is.factor(y[[1]]))
    coefs <- -coefs
  tibble(
    ..level = names(coefs),
    ..value = unname(coefs)
  )
}

#' @importFrom dplyr tibble mutate filter left_join %>% arrange 
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

#' @import rlang
#' @importFrom recipes bake prep
#' @importFrom purrr map
#' @export
bake.step_lencode_mixed <- function(object, newdata, ...) {
  for (col in names(object$mapping))
    newdata[, col] <- map_lme_coef(newdata[, col], object$mapping[[col]])

  newdata
}

#' @importFrom recipes printer
#' @export
print.step_lencode_mixed <-
  function(x, width = max(20, options()$width - 31), ...) {
    cat("Linear embedding for factors via mixed effects for ", sep = "")
    printer(names(x$mapping), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @importFrom dplyr bind_rows
#' @importFrom recipes is_trained
#' @importFrom broom tidy
#' @rdname step_lencode_mixed
#' @param x A `step_lencode_mixed` object.
#' @export
tidy.step_lencode_mixed <- function(x, ...) {
  if (is_trained(x)) {
    for(i in seq_along(x$mapping))
      x$mapping[[i]]$terms <- names(x$mapping)[i]
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
  res
}

#' @importFrom utils globalVariables
utils::globalVariables(c("..level", "..order"))
