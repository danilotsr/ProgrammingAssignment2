## Returns a list that encapsulates a matrix and the inverse of that matrix if cached.

## One should use the `cacheSolve` function to calculate/retrieve the inverse of the
## underlying matrix because it will use a previously cached result if available.

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## It calculates the inverse of the matrix stored in x, available at x$get().
## If the inverse of that matrix has been calculated and cached before, available
## at x$getInverse(), then the cached result is returned and the inverse operation
## is skipped.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (is.null(inv)) {
    inv <- solve(x$get(), ...)
    x$setInverse(inv)
  }
  inv
}
