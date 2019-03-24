## 2019-03-24
## CJ
## These functions attempt to copy what our teacher gave us
## for storing and returning the mean of a vector
## but instead return or store the inverse of a matrix

## Function attempts to build a list of functions used to 
## create and store the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_solve <- function(solve) m <<- solve
  get_solve <- function() m
  
  ## build the above functions into a list of functions
  list(set = set, get = get, set_solve = set_solve, get_solve = get_solve)
  
}


## Write a short comment describing this function
## function attempts to retrieve the inverse of a matrix.  
## if we don't have one stored, it will solve it and then store it
## and then return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  ## check if we've already stored the inverse
  
  m <- x$get_solve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ## solve the matrix and save it if we don't already have it stored
  data <- x$get()
  m <- solve(data, ...)
  x$set_solve(m)
  m
}
