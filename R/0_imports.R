#' @import recipes 
#' @import rlang
#' @importFrom utils globalVariables capture.output packageVersion stack
#' @importFrom uwot umap_transform umap
#' @importFrom lme4 glmer lmer
#' @importFrom rstanarm stan_glmer
#' @importFrom keras keras_model_sequential layer_embedding layer_flatten
#' @importFrom keras layer_dense compile fit get_layer backend keras_model
#' @importFrom keras layer_concatenate layer_input
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

utils::globalVariables(
  c(
    "Feature", "Missing", "No", "Node", "Split", "Yes",
    "training", "col_names", "y_name",
    "n", "p", "predictor", "summary_outcome", "value", "woe", "select", 
    "variable", ".",
    "type", "loss", "epochs", "..level", "..order"
    )
  )
