makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <<- function(y) {
    x <<- y
    invrs <- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {invrs <<- inverse}
  getInverse <- function() {invrs}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  invrs <- x$getInverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  mat <- x$get()
  invrs <- solve(mat, ...)
  x$setInverse(invrs)
  invrs
}

