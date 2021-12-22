## Calculation of a matrix's inverse using a cache.

## Create a cache matrix with methods to set and get the matrix
## and to set and get its inverse,
## Input: A matrix
## Output: A list with the set, get, getInverse and setInverse functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Create a cache matrix with methods to set and get the matrix
## and to set and get its inverse,
## Input: A matrix
## Output: A list with the set, get, getInverse and setInverse functions

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
