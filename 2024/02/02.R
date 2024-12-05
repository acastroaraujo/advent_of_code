
# Set up -----------------------------------------------------------------

library(purrr)

is_safe <- function(x) {
  if (length(unique(sign(diff(x)))) > 1) return(FALSE)
  if (!all(abs(diff(x)) >= 1 & abs(diff(x)) <= 3)) return(FALSE)
  return(TRUE)
}

input <- readLines("2024/02/input.txt") |> 
  map(\(x) as.numeric(unlist(strsplit(x, " "))))

# Part 1 -----------------------------------------------------------------

out <- map_lgl(input, is_safe)
sum(out)

# Part 2 -----------------------------------------------------------------

out <- map_lgl(input, function(x) {
  if (is_safe(x)) return(TRUE)
  if (any(apply(combn(x, length(x) - 1), 2, is_safe))) return(TRUE)
  return(FALSE)
})

sum(out)
