

## This function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { #set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x #get the value of the matrix
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  m <- x$getSolve() #set the value of the inverse matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() #get the value of the inverse matrix
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
