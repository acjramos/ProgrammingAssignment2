## This functions accepts a matrix as the input, computes the inverse
## of the matrix if it doesn't exist yet, then outputs the inverse of the input matrix. 

## The first function creates a matrix in which the inverse is to be computed.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function solves for the inverse of a matrix if it is not found in the cache.

cacheSolve <- function(x, ...) {
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
        ## Return a matrix that is the inverse of 'x'
}