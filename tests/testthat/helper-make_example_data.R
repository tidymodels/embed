rand_strings <- function(n_terms, samples) {
  strings <- vector(mode = "list", length = n_terms)
  strings <- vapply(
    strings,
    function(x) {
      x <- sample(letters, rpois(1, lambda = 5) + 3, replace = TRUE)
      paste0(x, collapse = "")
    },
    character(1)
  )
  sample(strings, size = samples, replace = TRUE)
}

set.seed(35674)
ex_dat <- data.frame(
  x1 = rnorm(500),
  x2 = rep(letters[1:2], each = 250),
  x3 = rand_strings(75, 500),
  x4 = rep(LETTERS[1:5], each = 100),
  stringsAsFactors = TRUE
)
ex_dat_ch <- ex_dat
ex_dat_ch$x3 <- as.character(ex_dat_ch$x3)

new_dat <- data.frame(
  x1 = 1:3,
  x2 = letters[c(1, 1, 2)],
  x3 = c("new_level_1", levels(ex_dat$x3)[1], NA_character_),
  x4 = LETTERS[1:3],
  stringsAsFactors = TRUE
)

new_dat_ch <- new_dat
new_dat_ch$x3 <- as.character(new_dat_ch$x3)
