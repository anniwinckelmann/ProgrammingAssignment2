## The first function creates a special matrix object from matrix x
## that can cache its inverse matrix. The second function computes
## the inverse of the special matrix or retrieves it from the cache.

## Creates special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ma <- NULL
  set <- function(y) {
    x <<- y
    ma <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) ma <<- matrix
  getmatrix <- function() ma
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## Computes or retrieves from the cache the inverse of special matrix created in makeCacheMatrix

cacheSolve <- function(x, ...) {
    ma <- x$getmatrix()
    if(!is.null(ma)) {
      message("getting cached data")
      ma
    }
    data <- x$get()
    ma <- solve(data, ...)
    x$setmatrix(ma)
    ma
  }
