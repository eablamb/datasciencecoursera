## makeCacheMatrix, cacheSolve
## Functions for caching the inverse of a matrix

## makeCacheMatrix - Returns a closure associated with matrix `y` with 
##                   the ability to cache the inverse of `y`.  This closure 
##                   has the following methods:
##
## set(y) - set the matrix object whose inverse will be cached
## get() - retrieve the stored matrix
## setInverse(inverse) - cache the inverse of the stored matrix `y`
## getInverse() - retrieve the cached inverse of `y`
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list (set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## cacheSolve - returns the cached inverse of the stored matrix in `x`
##
## If `x` does not have a cached inverse associated with it,
## one will be computed, stored, and returned.  On subsequent calls, 
## the cached inverse will not be recomputed.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m 
}

