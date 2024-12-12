
# Set up ------------------------------------------------------------------

A <- readLines("2024/08/input3.txt")
A <- do.call(rbind, strsplit(A, ""))

O <- A

A[which(A == "#")] <- "."


## What you want to do is calculate the equation for the line between two 
## points and then extrapolate and add anti node if inside the bounds of 
## the matrix.

a <- which(A == "a", arr.ind = TRUE)
a

ii <- combn(1:nrow(a), 2, simplify = FALSE)
xx <- do.call(rbind, ii) 
colnames(xx) <- c("i", "j")



purrr::pmap(as.data.frame(xx), function(i, j) {
  
  browser()
  
  m <- abs(a[j, ] - a[i, ])
  
  ## this generates 4 numbers, 2 of them repeated
  ## but I need the non-repeated.
  
  a[j, ] + m
  a[j, ] - m
  a[i, ] + m
  a[i, ] - m
  
  
  
})


