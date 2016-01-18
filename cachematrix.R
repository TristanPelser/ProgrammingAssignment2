## The pair of functions below cache the inverse of a matrix. This helps to save on
## computation time. 

## makeCacheMatrix creates a 'special matrix' object that can cache its own inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) inv <<- Inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve solves the inverse of a cached matrix, created using makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data1 <- x$get()
  inv <- solve(data1, ...)
  x$setInverse(inv)
  inv
}
