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
#' 
#' data(okc)
#' 
#' reencoded <- recipe(Class ~ age + location, data = okc) %>%
#'   step_lencode_bayes(location, outcome = vars(Class))
#' 
#' # See https://tidymodels.github.io/embed/ for examples

#' @importFrom recipes add_step step terms_select sel2char ellipse_check rand_id
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
    if (is.null(outcome))
      stop("Please list a variable in `outcome`", call. = FALSE)
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
        id = id
      )
    )
  }

step_lencode_bayes_new <-
  function(terms, role, trained, outcome, options, verbose, mapping, skip, id) {
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
      id = id
    )
  }

#' @importFrom recipes check_type
#' @export
prep.step_lencode_bayes <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names], quant = FALSE)
  y_name <- terms_select(x$outcome, info = info)
  res <-
    map(training[, col_names], stan_coefs, y = training[, y_name],
        x$options, x$verbose)
  step_lencode_bayes_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outcome = x$outcome,
    options = x$options,
    verbose = x$verbose,
    mapping = res,
    skip = x$skip,
    id = x$id
  )
}

#' @importFrom stats as.formula glm binomial coef gaussian na.omit
#' @importFrom dplyr bind_cols as_tibble
glm_coefs <- function(x, y, ...) {
  fam <- if(is.factor(y[[1]])) binomial else gaussian
  form <- as.formula(paste0(names(y), "~ 0 + value"))
  mod <-
    glm(
      form,
      data = bind_cols(as_tibble(x), y),
      family = fam,
      na.action = na.omit,
      ...
    )
  
  coefs <- coef(mod)
  names(coefs) <- gsub("^value", "", names(coefs))
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
#' @importFrom utils capture.output
#' @importFrom rstanarm stan_glmer
#' @importFrom tibble rownames_to_column 
#' @importFrom rlang set_names
#' @importFrom dplyr bind_rows
stan_coefs <- function(x, y, options, verbose, ...) {
  fam <- if (is.factor(y[[1]]))
    binomial()
  else
    gaussian()
  form <- as.formula(paste0(names(y), "~ (1|value)"))
  args <-
    list(
      form,
      data = bind_cols(as_tibble(x), y),
      family = fam,
      na.action = na.omit
    )
  if (length(options) > 0)
    args <- c(args, options)
  if (!verbose) {
    junk <- capture.output(mod <- do.call("stan_glmer", args))
  } else {
    mod <- do.call("stan_glmer", args)
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
  if (is.factor(y[[1]]))
    coefs$..value <- -coefs$..value
  coefs
}

#' @importFrom dplyr tibble mutate filter left_join %>% arrange 
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

#' @import rlang
#' @importFrom recipes bake prep
#' @importFrom purrr map
#' @export
bake.step_lencode_bayes <- function(object, new_data, ...) {
  for (col in names(object$mapping))
    new_data[, col] <- map_glm_coef(new_data[, col], object$mapping[[col]])

  new_data
}

#' @importFrom recipes printer
#' @export
print.step_lencode_bayes <-
  function(x, width = max(20, options()$width - 31), ...) {
    cat("Linear embedding for factors via GLM for ", sep = "")
    printer(names(x$mapping), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @importFrom dplyr bind_rows
#' @importFrom recipes is_trained
#' @rdname step_lencode_bayes
#' @param x A `step_lencode_bayes` object.
#' @export
#' @export tidy.step_lencode_bayes
tidy.step_lencode_bayes <- function(x, ...) {
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
  res$id <- x$id
  res
}

#' @importFrom utils globalVariables
utils::globalVariables(c("..level", "..order"))
