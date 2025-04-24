#' Dummy Variables Creation via Feature Hashing
#'
#' @description 
#' `r lifecycle::badge("deprecated")`
#'
#' Please use [textrecipes::step_dummy_hash()] instead.
#' 
#' @keywords internal
#' @export
step_feature_hash <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  num_hash = 2^6,
  preserve = deprecated(),
  columns = NULL,
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("feature_hash")
) {
  lifecycle::deprecate_stop(
    "0.2.0",
    "embed::step_feature_hash()",
    "textrecipes::step_dummy_hash()"
  )
}
