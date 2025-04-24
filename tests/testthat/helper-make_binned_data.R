library(dplyr)

sim_data_2class <- function(n = 300) {
  xf <- function(x) {
    case_when(
      x < 0.33 ~ "a",
      x < 0.66 & x >= 0.33 ~ "b",
      TRUE ~ "a"
    )
  }

  dat <- tibble(x = runif(n), z = runif(n)) |>
    mutate(
      class = purrr::map_chr(x, xf),
      class = factor(class, levels = letters[1:2])
    )
  dat
}

sim_data_3class <- function(n = 300) {
  xf <- function(x) {
    case_when(
      x < 0.33 ~ "a",
      x < 0.66 & x >= 0.33 ~ "b",
      TRUE ~ "c"
    )
  }

  dat <- tibble(x = runif(n), z = runif(n)) |>
    mutate(
      class = purrr::map_chr(x, xf),
      class = factor(class, levels = letters[1:3])
    )
  dat
}

sim_data_reg <- function(n = 300) {
  xf <- function(x) {
    case_when(
      x < 0.33 ~ 1,
      x < 0.66 & x >= 0.33 ~ 3,
      TRUE ~ 5
    )
  }

  dat <- tibble(x = runif(n), z = runif(n)) |>
    mutate(
      y = purrr::map_dbl(x, xf),
      y = y + rnorm(n, sd = .1)
    )
  dat
}
