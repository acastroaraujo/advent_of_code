
# Set up ------------------------------------------------------------------

M <- readLines("2024/06/input.txt")
M <- do.call(rbind, strsplit(M, ""))

# Functions ---------------------------------------------------------------

move <- function(x) {
  switch(x,
    "^" = c(-1, 0),
    ">" = c(0, 1),
    "v" = c(1, 0),
    "<" = c(0, -1)
  )
}
  
is_blocked <- function(x) {
  if (x == "#") return(TRUE)
  FALSE
}

is_outside <- function(arr) {
  any(arr < c(1, 1)) | any(arr > dim(M))
}

rotate <- function(x) {
  switch(x,
    "^" = ">",
    ">" = "v",
    "v" = "<",
    "<" = "^"
  )
}

# Part 1 ------------------------------------------------------------------

s <- "^"
history <- list(which(M == s, arr.ind = TRUE))
new <- history[[length(list)]] + move(s)

repeat {
  if (is_outside(new)) break
  if (is_blocked(M[new])) {
    s <- rotate(s)
    new <- history[[length(history)]] + move(s)
    next
  }
  history[[length(history) + 1]] <- new
  new <- history[[length(history)]] + move(s)
}

output <- unique(do.call(rbind, history))

message(nrow(output))

# Part 2 ------------------------------------------------------------------

grids <- apply(output[-1, ], 1, function(x) {
  M[rbind(x)] <- "#"
  M
}, simplify = FALSE)

out <- 0L

for (i in 1:length(grids)) {
  
  A <- grids[[i]]
  s <- "^"
  init <- which(A == s, arr.ind = TRUE)
  new <- init + move(s)
  
  cat("iteration:", i, "of", length(grids), "\r")
  
  repeat {
    if (is_outside(new)) break
    if (is_blocked(A[new])) {
      new <- new - move(s)
      s <- rotate(s)
      new <- new + move(s)
    }
    
    if (A[new] == s) {
      out <- out + 1L
      break
    }
    
    A[new] <- s
    new <- new + move(s)
  }
}

message(out)
