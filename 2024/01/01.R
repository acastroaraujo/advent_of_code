

# Set up -----------------------------------------------------------------
library(tidyverse)
d <- read_table("2024/01/input.txt", col_names = letters[1:2])

# Part 1 -----------------------------------------------------------------

d |> 
  apply(MARGIN = 2, sort) |> 
  apply(MARGIN = 1, \(x) abs(diff(x))) |> 
  sum()

# Part 2 -----------------------------------------------------------------

lookup <- table(d$b)
i <- as.character(d$a[d$a %in% d$b])

lookup[i] |> 
  enframe("a", "b") |> 
  mutate_all(as.numeric) |> 
  summarize(similarity = sum(a * b))

