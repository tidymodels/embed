#' Supervised Factor Conversions into Linear Functions using Likelihood Encodings
#'
#' `step_lencode_glm` creates a *specification* of a recipe step that
#'  will convert a nominal (i.e. factor) predictor into a single set of
#'  scores derived from a generalized linear model.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_lencode_glm`, this indicates the variables to be encoded
#'  into a numeric format. See [recipes::selections()] for more details. For
#'  the `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param outcome A call to `vars` to specify which variable is
#'  used as the outcome in the generalized linear model. Only
#'  numeric and two-level factors are currently supported.
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
#' # Tidying
#' 
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns
#' `terms` (the selectors or variables selected), `value` and `component` is
#' returned.
#' 
#' @template case-weights-supervised
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
#'   step_lencode_glm(sponsor_code, outcome = vars(class))
#' }
step_lencode_glm <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           outcome = NULL,
           mapping = NULL,
           skip = FALSE,
           id = rand_id("lencode_glm")) {
    if (is.null(outcome)) {
      rlang::abort("Please list a variable in `outcome`")
    }
    add_step(
      recipe,
      step_lencode_glm_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        outcome = outcome,
        mapping = mapping,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }

step_lencode_glm_new <-
  function(terms, role, trained, outcome, mapping, skip, id, case_weights) {
    step(
      subclass = "lencode_glm",
      terms = terms,
      role = role,
      trained = trained,
      outcome = outcome,
      mapping = mapping,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }


#' @export
prep.step_lencode_glm <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  
  wts <- recipes::get_case_weights(info, training)
  were_weights_used <- recipes::are_weights_used(wts)
  if (isFALSE(were_weights_used) || is.null(wts)) {
    wts <- NULL
  }
  
  if (length(col_names) > 0) {
    check_type(training[, col_names], quant = FALSE)
    y_name <- recipes::recipes_eval_select(x$outcome, training, info)
    res <- map(
      training[, col_names],
      glm_coefs, 
      y = training[, y_name],
      wts = wts
    )
  } else {
    res <- list()
  }
  step_lencode_glm_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outcome = x$outcome,
    mapping = res,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}


glm_coefs <- function(x, y, wts = NULL, ...) {
  fam <- if (is.factor(y[[1]])) binomial else gaussian
  form <- as.formula(paste0(names(y), "~ 0 + value"))

  if (is.vector(x) | is.factor(x)) {
    x <- tibble(value = x)
  } else {
    x <- as_tibble(x)
  }

  mod <-
    glm(
      form,
      data = bind_cols(x, y),
      family = fam,
      weights = wts,
      na.action = na.omit,
      ...
    )

  coefs <- coef(mod)
  names(coefs) <- gsub("^value", "", names(coefs))
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



map_glm_coef <- function(dat, mapping) {
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
bake.step_lencode_glm <- function(object, new_data, ...) {
  check_new_data(names(object$mapping), object, new_data)
  
  for (col in names(object$mapping)) {
    new_data[, col] <- map_glm_coef(new_data[, col], object$mapping[[col]])
  }

  new_data
}

#' @export
print.step_lencode_glm <-
  function(x, width = max(20, options()$width - 31), ...) {
    title <- "Linear embedding for factors via GLM for "
    print_step(names(x$mapping), x$terms, x$trained, title, width,
               case_weights = x$case_weights)
    invisible(x)
  }


#' @rdname tidy.recipe
#' @param x A `step_lencode_glm` object.
#' @export
tidy.step_lencode_glm <- function(x, ...) {
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
required_pkgs.step_lencode_glm <- function(x, ...) {
  c("embed")
}
