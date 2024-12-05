
# Set up -----------------------------------------------------------------

library(tidyverse)

X <- readLines("2024/04/input.txt")
X <- do.call(rbind, strsplit(X, ""))

# Part 1 -----------------------------------------------------------------

search_xmas <- function(x) {
  x <- paste(x, collapse = "")
  sum(str_count(x, c("XMAS", "SAMX")))
}

extract_diagonals <- function(M) {
  d <- dim(M)
  x <- seq(-d[[2]] + 1, d[[1]] - 1)                      # I cheated
  z <- seq(2, d[[1]] * 2)                                # to get
  diags <- lapply(x, \(x) M[row(M) == (col(M) + x)])     # these
  anti_diags <- lapply(z, \(x) M[row(M) + col(M) == x])  # lines.
  append(diags, anti_diags)
}

h <- apply(X, 1, search_xmas)
v <- apply(X, 2, search_xmas)
d <- map_dbl(extract_diagonals(X), search_xmas)

sum(h, v, d)

# Part 2 ------------------------------------------------------------------

ij <- expand_grid(
  i = map(0:(nrow(X) - 3), \(x) x + 1:3),
  j = map(0:(ncol(X) - 3), \(x) x + 1:3),
)

out <- pmap_lgl(ij, function(i, j) {
  a <- paste(diag(X[i, j]), collapse = "")
  b <- paste(diag(X[rev(i), j]), collapse = "")
  str_detect(a, "MAS|SAM") && str_detect(b, "MAS|SAM")
}) 

sum(out)

