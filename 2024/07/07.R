
library(purrr)
library(furrr)
future::plan(future::multisession)

lines <- readLines("2024/07/input.txt") |> 
  stringr::str_extract_all("\\d+") |> 
  map(as.numeric)

output <- map_dbl(lines, pluck, 1)
input <- map(lines, \(x) x[-1])

# Part 1 ------------------------------------------------------------------

run <- function(x, target) {
  
  if (x[[1]] > target | !is.finite(x[[1]])) return(FALSE)
  if (length(x) == 1) return(ifelse(x == target, TRUE, FALSE))
  
  out <- detect_index(c("+", "*"), function(f) {
    r <- do.call(f, as.list(x[1:2]))
    run(c(r, x[-(1:2)]), target)
  }) 
  
  out > 0
}

i <- furrr::future_map2_lgl(input, output, run, .progress = TRUE) 
sum(output[i])

# Part 2 ------------------------------------------------------------------

run <- function(x, target) {
  
  if (x[[1]] > target | !is.finite(x[[1]])) return(FALSE)
  if (length(x) == 1) return(ifelse(x == target, TRUE, FALSE))
  
  `%c%` <- function(a, b) as.numeric(paste0(a, b))
  
  out <- detect_index(c("+", "*", "%c%"), function(f) {
    r <- do.call(f, as.list(x[1:2]))
    run(c(r, x[-(1:2)]), target)
  }) 
  
  out > 0
}

i <- furrr::future_map2_lgl(input, output, run, .progress = TRUE) 
format(sum(output[i]), scientific = FALSE)

