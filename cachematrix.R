## Matrix inversion is a frequently used but
## computationally expensive operation.
## Therefore, it makes sense to memoize it.

## Creates a wrapper for matrix() that can store
## its inverse in addition to its own entries,
## exposing both via getter and setter functions.
makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL

  list(
    set = function(y) {
      x <<- y
      xinv <<- NULL
    },
    get = function() x,
    setinv = function(inv) xinv <<- inv,
    getinv = function() xinv
  )
}


## A wrapper for solve() compatible with
## makeCacheMatrix(). Computes the inverse once
## and stores it in the matrix. Subsequent
## calls retrieve the cached value.
cacheSolve <- function(x, ...) {
  xinv <- x$getinv()
  if (!is.null(xinv)) {
    message("Fetching from cache...")
    xinv
  }

  data <- x$get()
  xinv <- solve(data, ...)
  x$setinv(xinv)

  xinv
}
