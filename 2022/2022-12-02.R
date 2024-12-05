
# Day 2 - Advent of Code 2022
# https://adventofcode.com/2022/day/2

library(tidyverse) 

# helper ------------------------------------------------------------------

result <- function(opponent, self) {
  
  if (wins[self] == opponent) {
    return(6L)
  }
  
  if (opponent == self) {
    return(3L)
  }
  
  if (loses[self] == opponent) {
    return(0L)
  }
  
}


# lookup tables -----------------------------------------------------------

# these are named vectors, they're a little similar to dictionaries in Python

wins <- c(
  "rock" = "scissors",
  "paper" = "rock",
  "scissors" = "paper"
)

loses <- setNames(names(defeats), defeats)

name_lookup <- c(
  "A" = "rock", 
  "B" = "paper", 
  "C" = "scissors",
  "X" = "rock",
  "Y" = "paper",
  "Z" = "scissors"
)

shape_score <- c("rock" = 1L, "paper" = 2L, "scissors" = 3L)

# input data --------------------------------------------------------------

url <- "https://raw.githubusercontent.com/juanrloaiza/advent-of-code-2022/main/02-rock_paper_scissors/input.txt"
plays <- read_delim(url, col_names = c("opponent", "self"))

# results -----------------------------------------------------------------

plays <- plays |> 
  mutate(across(everything(), \(x) name_lookup[x])) |> 
  mutate(shape = shape_score[self]) |> 
  mutate(result = map2_int(opponent, self, result)) |> 
  mutate(total_score = shape + result)

sum(plays$total_score)

