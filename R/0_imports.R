#' @import recipes 
#' @import rlang
#' @importFrom utils globalVariables capture.output packageVersion stack compareVersion
#' @importFrom uwot umap_transform umap
#' @importFrom keras keras_model_sequential layer_embedding layer_flatten
#' @importFrom keras layer_dense compile fit get_layer backend keras_model
#' @importFrom keras layer_concatenate layer_input
#' @importFrom lifecycle deprecated
#' @importFrom stats as.formula glm binomial coef gaussian na.omit
#' @importFrom stats setNames model.matrix complete.cases
#' @importFrom purrr map
#' @importFrom tibble rownames_to_column as_tibble tibble
#' @importFrom dplyr bind_cols bind_rows mutate filter left_join %>% arrange 
#' @importFrom dplyr ends_with contains one_of
#' @importFrom dplyr tibble mutate filter left_join %>% arrange 
#' @importFrom tidyr gather
#' @importFrom withr with_seed


# ------------------------------------------------------------------------------

#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics required_pkgs
#' @export
generics::required_pkgs

#' @importFrom generics tunable
#' @export
generics::tunable

# ------------------------------------------------------------------------------

utils::globalVariables(
  c(
    "Feature", "Missing", "No", "Node", "Split", "Yes",
    "training", "col_names", "y_name",
    "n", "p", "predictor", "summary_outcome", "value", "woe", "select", 
    "variable", ".",
    "type", "loss", "epochs", "..level", "..order",
    "data", "n_tot"
    )
  )

# from recipes
is_tune <- function (x)  {
  if (is.call(x)) {
    if (rlang::call_name(x) == "tune") {
      return(TRUE)
    }
    else {
      return(FALSE)
    }
  }
  else {
    return(FALSE)
  }
  FALSE
}

prop2int <- function (x, p) {
  cuts <- seq(0, p, length.out = p + 1)
  as.integer(cut(x * p, breaks = cuts, include.lowest = TRUE))
}
