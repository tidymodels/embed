# nocov start

# Global vars ------------------------------------------------------------------

utils::globalVariables(
  c(
    ".",
    "..level",
    "..order",
    "..value",
    ".group",
    ".rows",
    "col_names",
    "data",
    "epochs",
    "Feature",
    "group",
    "group_f",
    "loss",
    "Missing",
    "n",
    "n_tot",
    "No",
    "Node",
    "p",
    "predictor",
    "select",
    "Split",
    "summary_outcome",
    "terms",
    "training",
    "type",
    "value",
    "var",
    "variable",
    "where",
    "woe",
    "y_name",
    "Yes",
    "var_outcome"
  )
)

# adapted from ps:::is_cran_check()
is_cran_check <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    FALSE
  } else {
    Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""
  }
}
# nocov end
