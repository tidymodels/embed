#' Weight of evidence transformation
#'
#' `step_woe` creates a *specification* of a
#'  recipe step that will transform nominal data into its numerical
#'  transformation based on weights of evidence against a binary outcome.
#'
#' @inheritParams step_lencode_bayes
#' @inherit step_center return
#' @param ... One or more selector functions to choose which
#'  variables will be used to compute the components. See
#'  [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new woe components columns created by the original
#'  variables will be used as predictors in a model.
#' @param outcome The bare name of the binary outcome encased in `vars()`.
#' @param outcome The bare name of the binary outcome encased in `vars()`.
#' @param dictionary A tbl. A map of levels and woe values. It must
#' have the same layout than the output returned from [dictionary()].
#' If `NULL`` the function will build a dictionary with those variables
#' passed to \code{...}. See [dictionary()] for details.
#' @param Laplace The Laplace smoothing parameter. A value usually
#' applied to avoid -Inf/Inf from predictor category with only one
#' outcome class. Set to 0 to allow Inf/-Inf. The default is 1e-6.
#' Also known as 'pseudocount' parameter of the Laplace smoothing technique.
#' @param prefix A character string that will be the prefix to the
#'  resulting new variables. See notes below.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with the woe dictionary used to map
#'  categories with woe values.
#' @keywords datagen
#' @concept preprocessing woe transformation_methods
#' @export
#' @details
#' WoE is a transformation of a group of variables that produces
#' a new set of features. The formula is
#'
#' \deqn{woe_c = log((P(X = c|Y = 1))/(P(X = c|Y = 0)))}
#'
#' where \eqn{c} goes from 1 to \eqn{C} levels of a given nominal
#' predictor variable \eqn{X}.
#'
#' These components are designed to transform nominal variables into
#' numerical ones with the property that the order and magnitude
#' reflects the association with a binary outcome.  To apply it on
#' numerical predictors, it is advisable to discretize the variables
#' prior to running WoE. Here, each variable will be binarized to
#' have woe associated later. This can achieved by using [step_discretize()].
#'
#' The argument `Laplace` is an small quantity added to the
#' proportions of 1's and 0's with the goal to avoid log(p/0) or
#' log(0/p) results. The numerical woe versions will have names that
#' begin with `woe_` followed by the respective original name of the
#' variables. See Good (1985).
#'
#' One can pass a custom `dictionary` tibble to \code{step_woe()}.
#' It must have the same structure of the output from
#' \code{dictionary()} (see examples). If not provided it will be
#' created automatically. The role of this tibble is to store the map
#' between the levels of nominal predictor to its woe values. You may
#' want to tweak this object with the goal to fix the orders between
#' the levels of one given predictor. One easy way to do this is by
#' tweaking an output returned from \code{dictionary()}.
#'
#' @references Kullback, S. (1959). *Information Theory and Statistics.* Wiley, New York.
#'
#' Hastie, T., Tibshirani, R. and Friedman, J. (1986). *Elements of Statistical Learning*, Second Edition, Springer, 2009.
#'
#' Good, I. J. (1985), "Weight of evidence: A brief survey", _Bayesian Statistics_, 2, pp.249-270.
#'
#' @examples
#' library(modeldata)
#' data("credit_data")
#'
#' set.seed(111)
#' in_training <- sample(1:nrow(credit_data), 2000)
#'
#' credit_tr <- credit_data[ in_training, ]
#' credit_te <- credit_data[-in_training, ]
#'
#' rec <- recipe(Status ~ ., data = credit_tr) %>%
#'   step_woe(Job, Home, outcome = vars(Status))
#'
#' woe_models <- prep(rec, training = credit_tr)
#'
#' # the encoding:
#' bake(woe_models, new_data = credit_te %>% slice(1:5), starts_with("woe"))
#' # the original data
#' credit_te %>% slice(1:5) %>% dplyr::select(Job, Home)
#' # the details:
#' tidy(woe_models, number = 1)
#'
#' # Example of custom dictionary + tweaking
#' # custom dictionary
#' woe_dict_custom <- credit_tr %>% dictionary(Job, Home, outcome = "Status")
#' woe_dict_custom[4, "woe"] <- 1.23 #tweak
#'
#' #passing custom dict to step_woe()
#' rec_custom <- recipe(Status ~ ., data = credit_tr) %>%
#'   step_woe(Job, Home, outcome = vars(Status), dictionary = woe_dict_custom) %>%
#'   prep
#'
#' rec_custom_baked <- bake(rec_custom, new_data = credit_te)
#' rec_custom_baked %>% dplyr::filter(woe_Job == 1.23) %>% head
#'
step_woe <- function(recipe,
                     ...,
                     role = "predictor",
                     outcome,
                     trained = FALSE,
                     dictionary = NULL,
                     Laplace = 1e-6,
                     prefix = "woe",
                     skip = FALSE,
                     id = rand_id("woe")) {
  if (missing(outcome)) {
    rlang::abort('argument "outcome" is missing, with no default')
  }
  
  add_step(
    recipe,
    step_woe_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      outcome = outcome,
      dictionary = dictionary,
      Laplace = Laplace,
      prefix = prefix,
      skip = skip,
      id = id
    )
  )
}

## Initializes a new object
step_woe_new <- function(terms, role, trained, outcome, dictionary, Laplace, prefix, skip, id) {
  step(
    subclass = "woe",
    terms = terms,
    role = role,
    trained = trained,
    outcome = outcome,
    dictionary = dictionary,
    Laplace = Laplace,
    prefix = prefix,
    skip = skip,
    id = id
  )
}

#' Crosstable with woe between a binary outcome and a predictor variable.
#'
#' Calculates some summaries and the WoE (Weight of Evidence) between a binary
#' outcome and a given predictor variable. Used to biuld the dictionary.
#'
#' @param predictor A atomic vector, usualy with few distinct values.
#' @param outcome The dependent variable. A atomic vector with exactly 2 distinct values.
#' @param Laplace The `pseudocount` parameter of the Laplace Smoothing
#' estimator. Default to 1e-6. Value to avoid -Inf/Inf from predictor category with only
#' one outcome class. Set to 0 to allow Inf/-Inf.
#'
#' @return a tibble with counts, proportions and woe.
#'  Warning: woe can possibly be -Inf. Use 'Laplace' arg to avoid that.
#'
#'
#' @references Kullback, S. (1959). *Information Theory and Statistics.* Wiley, New York.
#'
#' Hastie, T., Tibshirani, R. and Friedman, J. (1986). *Elements of Statistical Learning*, Second Edition, Springer, 2009.
#'
#' Good, I. J. (1985), "Weight of evidence: A brief survey", _Bayesian Statistics_, 2, pp.249-270.
woe_table <- function(predictor, outcome, Laplace = 1e-6) {
  outcome_original_labels <- unique(outcome)
  
  if (length(outcome_original_labels) != 2) {
    rlang::abort(sprintf("'outcome' must have exactly 2 categories (has %s)",
                 length(outcome_original_labels)))
  }
  
  if (is.factor(predictor)) {
    predictor <- as.character(predictor)
  }
  
  woe_expr <- parse(
    text = sprintf(
      "log(((n_%s + Laplace)/(sum(n_%s) + 2 * Laplace))/((n_%s + Laplace)/(sum(n_%s) + 2 * Laplace)))",
      outcome_original_labels[1],
      outcome_original_labels[1],
      outcome_original_labels[2],
      outcome_original_labels[2]
    )
  )
  
  woe_tbl <-
    tibble::tibble(outcome, predictor) %>%
    dplyr::group_by(outcome, predictor) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::group_by(predictor) %>%
    dplyr::mutate(n_tot = sum(n)) %>%
    dplyr::group_by(outcome) %>%
    dplyr::mutate(p = n/sum(n)) %>%
    tidyr::gather(summary, value, n, p) %>%
    tidyr::unite(summary_outcome, summary, outcome) %>%
    tidyr::spread(summary_outcome, value, fill = 0) %>%
    dplyr::mutate(
      woe = eval(woe_expr),
      predictor = as.character(predictor)
    )
  
  return(woe_tbl)
}


#' Weight of evidence dictionary
#'
#' Builds the woe dictionary of a set of predictor variables upon a given binary outcome.
#' Convenient to make a woe version of the given set of predictor variables and also to allow
#' one to tweak some woe values by hand.
#'
#' @param .data A tbl. The data.frame where the variables come from.
#' @param outcome The bare name of the outcome variable with exactly 2 distinct values.
#' @param ... bare names of predictor variables or selectors accepted by \code{dplyr::select()}.
#' @param Laplace Default to 1e-6. The `pseudocount` parameter of the Laplace Smoothing
#' estimator. Value to avoid -Inf/Inf from predictor category with only one outcome class.
#' Set to 0 to allow Inf/-Inf.
#'
#' @return a tibble with summaries and woe for every given predictor variable stacked up.
#'
#' @details You can pass a custom dictionary to \code{step_woe()}. It must have the exactly
#' the same structure of the output of [dictionary()]. One easy way to do this
#' is by tweaking an output returned from it.
#'
#' @examples
#'
#' mtcars %>% dictionary("am", cyl, gear:carb)
#'
#'
#' @references Kullback, S. (1959). *Information Theory and Statistics.* Wiley, New York.
#'
#' Hastie, T., Tibshirani, R. and Friedman, J. (1986). *Elements of Statistical Learning*, Second Edition, Springer, 2009.
#'
#' Good, I. J. (1985), "Weight of evidence: A brief survey", _Bayesian Statistics_, 2, pp.249-270.
#'

#' @export
dictionary <- function(.data, outcome, ..., Laplace = 1e-6) {
  outcome_vector <- .data %>% dplyr::pull(!!outcome)
  .data %>%
    dplyr::select(..., -!!outcome) %>%
    purrr::map(woe_table, outcome = outcome_vector, Laplace = Laplace) %>%
    dplyr::bind_rows(.id = "variable") %>% 
    mutate(outcome = outcome)
}


#' Add WoE in a data frame
#'
#' A tidyverse friendly way to plug WoE versions of a set of predictor variables against a
#' given binary outcome.
#'
#' @param .data A tbl. The data.frame to plug the new woe version columns.
#' @param outcome The bare name of the outcome variable.
#' @param ... Bare names of predictor variables, passed as you would pass variables to
#'  \code{dplyr::select()}. This means that you can use all the helpers like \code{starts_with()}
#'  and \code{matches()}.
#' @param dictionary A tbl. If NULL the function will build a dictionary with those variables
#'  passed to \code{...}. You can pass a custom dictionary too, see [dictionary()] for details.
#' @param prefix A character string that will be the prefix to the resulting new variables.
#'
#' @return A tibble with the original columns of .data plus the woe columns wanted.
#'
#' @details You can pass a custom dictionary to [add_woe()]. It must have the exactly the same
#'  structure of the output of [dictionary()]. One easy way to do this is to tweak a output
#'  returned from it.
#'
#' @examples
#'
#' mtcars %>% add_woe("am", cyl, gear:carb)
#'

#' @export
add_woe <- function(.data, outcome, ..., dictionary = NULL, prefix = "woe") {
  if (missing(.data)) {
    rlang::abort('argument ".data" is missing, with no default')
  }
  if (missing(outcome)) {
    rlang::abort('argument "outcome" is missing, with no default')
  }
  if (!is.character(outcome)) {
    rlang::abort("'outcome' should be a single character value.")
  }

  if (is.null(dictionary)) {
    dictionary <- dictionary(.data, outcome, ...)
  } else {
    if (is.null(dictionary$variable)) {
      rlang::abort('column "variable" is missing in dictionary.')
    }
    if (is.null(dictionary$predictor)) {
      rlang::abort('column "predictor" is missing in dictionary.')
    }
    if (is.null(dictionary$woe)) {
      rlang::abort('column "woe" is missing in dictionary.')
    }
  }
  
  # warns if there is variable with more than 50 levels
  level_counts <- table(dictionary$variable)
  purrr::walk2(
    level_counts,
    names(level_counts),
    ~ if(.x > 50)
      rlang::warn(
        paste0(
          "Variable ", .y, " has ", .x,
          " unique values. Is this expected? In case of numeric variable, ",
          "see ?step_discretize().")
        )
  )
  
  
  if (missing(...)) {
    dots_vars <- names(.data)
  } else {
    dots_vars <- names(.data %>% dplyr::select(...))
  }
  
  output <- dictionary %>%
    dplyr::filter(variable %in% dots_vars) %>%
    dplyr::select(variable, predictor, woe)
  
  # See https://tidyr.tidyverse.org/dev/articles/in-packages.html
  if (tidyr_new_interface()) {
    output <- tidyr::nest(output, woe_table = -dplyr::one_of("variable"))
  } else {
    output <- tidyr::nest(output, .key = "woe_table")
  }
  
  output <- 
    output %>%
    dplyr::mutate(
      woe_table =
        purrr::map2(woe_table, variable,
                    ~ purrr::set_names(.x, c(.y, paste0(prefix,"_", .y)))
        ) %>%
        purrr::set_names(variable)
    )
  
  output <- purrr::map2(
    output$woe_table,
    output$variable, ~ {
      .data %>%
        dplyr::select(!!.y) %>%
        dplyr::mutate_all(as.character) %>%
        dplyr::left_join(.x, by = .y) %>%
        dplyr::select(starts_with(prefix))
    }) %>%
    dplyr::bind_cols(.data, .) %>%
    tibble::as_tibble()
  
  output
}


#' @export
prep.step_woe <- function(x, training, info = NULL, ...) {
  outcome_name <- recipes::recipes_eval_select(x$outcome, training, info)
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  col_names <- col_names[!(col_names %in% outcome_name)]
  check_type(training[, col_names], quant = FALSE)
  check_type(training[, outcome_name], quant = FALSE)
  
  if (is.null(x$dictionary)) {
    x$dictionary <- dictionary(
      .data = training[, unique(c(outcome_name, col_names))],
      outcome = outcome_name
    ) %>% 
      dplyr::mutate(outcome = outcome_name)
  }
  
  step_woe_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outcome = x$outcome,
    dictionary = x$dictionary,
    Laplace = x$Laplace,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_woe <- function(object, new_data, ...) {
  dict <- object$dictionary
  woe_vars <- unique(dict$variable)
  new_data <- add_woe(
    .data = new_data,
    outcome = dict$outcome[1],
    dictionary = dict,
    prefix = object$prefix
  )
  new_data <- new_data[, !(colnames(new_data) %in% woe_vars), drop = FALSE]
  as_tibble(new_data)
}

#' @export
print.step_woe <- function(x, width = max(20, options()$width - 29), ...) {
  cat("WoE version against outcome", rlang::quo_text(x$outcome[[1]]), "for ")
  printer(unique(x$dictionary$variable), x$terms, x$trained, width = width)
  invisible(x)
}


#' @rdname step_woe
#' @param x A `step_woe` object.
#' @export
tidy.step_woe <- function(x, ...) {
  if (is_trained(x)) {
    res <- 
      x$dictionary %>% 
      dplyr::rename(terms = variable, value = predictor)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  value = rlang::na_chr,
                  ntot = rlang::na_int,
                  n_0 = rlang::na_int,
                  n_1 = rlang::na_int,
                  p_0 = rlang::na_dbl,
                  p_1 = rlang::na_dbl,
                  woe = rlang::na_dbl)
  }
  res$id <- x$id
  res
}

# ------------------------------------------------------------------------------


tidyr_new_interface <- function() {
  utils::packageVersion("tidyr") > "0.8.99"
}



#' @rdname required_pkgs.embed
#' @export
required_pkgs.step_woe <- function(x, ...) {
  c("embed")
}

