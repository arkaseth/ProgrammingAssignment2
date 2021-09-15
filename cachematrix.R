## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inver) inv <<- inver
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns inverse if it already exists.
## Solves, stores, and returns if it does not.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("returning cached value...")
    return(inv)
  }
  x_mat <- x$get()
  inv <- solve(x_mat, ...)
  x$setinv(inv)
  return(inv)
}
