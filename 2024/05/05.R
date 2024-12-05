
# Set up ------------------------------------------------------------------

library(tidyverse)

mid_point <- function(x) x[ceiling(length(x) / 2)]

check <- function(x) {
  input <- map_chr(combn(x, 2, simplify = FALSE), \(x) paste(x, collapse = "|"))
  input %in% rules
}

fix_order <- function(x) {
  
  input <- map_chr(combn(x, 2, simplify = FALSE), \(x) paste(x, collapse = "|"))
  ok <- input %in% rules
  if (all(ok)) return(x)
  ii <- input[!ok][[1]] 
  ii <- as.integer(str_split_1(ii, "\\|"))
  loc <- which(x %in% ii)
  x[loc] <- x[rev(loc)]
  
  fix_order(x)
}

input <- readLines("2024/05/input.txt")

i <- which(input == "")
rules <- input[1:(i - 1)]
updates <- input[(i + 1):length(input)]
updates <- map(updates, \(x) as.integer(str_split_1(x, ",")))

# Part 1 ------------------------------------------------------------------

ok <- map_lgl(updates, \(x) all(check(x)))
sum(map_dbl(updates[ok], mid_point))

# Part 2 ------------------------------------------------------------------

updates[!ok] |> 
  map(fix_order) |> 
  map_dbl(mid_point) |> 
  sum()
