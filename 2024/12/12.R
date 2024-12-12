
# Set up ------------------------------------------------------------------

library(purrr)
library(igraph)
X <- readLines("2024/12/input2.txt")
X <- do.call(rbind, strsplit(x = X, split = ""))

# Part 1 ------------------------------------------------------------------

get_price <- function(target) {
  
  if (!target %in% X) return(0)
  
  sub_arr <- which(X == target, arr.ind = TRUE)
  net <- igraph::make_empty_graph(n = nrow(sub_arr), directed = FALSE)
  
  for (i in seq_len(nrow(sub_arr) - 1)) {
    diff_sub_arr <- diff(sub_arr, i)
    n <- apply(abs(diff_sub_arr), 1, \(x) all(x == 0:1) || all(x == 1:0))
    n <- which(n)
    net <- igraph::add_edges(net, rbind(n, n + i))
  }
  
  out <- map_dbl(igraph::decompose(net), function(g) {
    igraph::vcount(g) * sum(4 - igraph::degree(g))
  })
  
  sum(out)
  
}

out <- map_dbl(LETTERS, get_price, .progress = TRUE)
sum(out)

# Part 2 ------------------------------------------------------------------

## Couldn't finish this part

contract_recursive <- function(g) {
  
  ns <- map_lgl(neighborhood(g), function(x) {
    length(unique(degree(g, v = x))) == 1
  })
  
  if (all(!ns) | length(ns) == 2) return(g)
  
  i <- detect_index(ns, isTRUE)
  mapping <- 1:vcount(g)
  mapping[i] <- neighbors(g, i)[[1]]
  g <- simplify(contract(g, mapping))
  g <- delete_vertices(g, which(degree(g) == 0))
  contract_recursive(g)
}

contract_recursive2 <- function(g) {
  
  i <- detect_index(1:vcount(g), function(x) {
    any(degree(g, x) == degree(g, neighbors(g, x)))
  })
  
  if (i == 0L) return(g)
  
  m <- neighbors(g, i)[degree(g, i) == degree(g, neighbors(g, i))]
  
  mapping <- 1:vcount(g)
  mapping[i] <- m
  
  g <- simplify(contract(g, mapping))
  g <- delete_vertices(g, which(degree(g) == 0))
  contract_recursive2(g)
}
