
# Set up -----------------------------------------------------------------

library(tidyverse)
input <- paste(readLines("2024/03/input.txt"), collapse = "")
mul <- `*`

# Part 1 -----------------------------------------------------------------

input |> 
  str_extract_all(pattern = r"{mul\(\d{1,3},\d{1,3}\)}") |> 
  flatten_chr() |> 
  map_dbl(\(x) eval(parse(text = x))) |> 
  sum()

# Part 2 -----------------------------------------------------------------

input |> 
  str_remove_all(pattern = r"{don't\(\).+?do\(\)}") |> 
  str_extract_all(pattern = r"{mul\(\d{1,3},\d{1,3}\)}") |> 
  flatten_chr() |> 
  map_dbl(\(x) eval(parse(text = x))) |> 
  sum()

