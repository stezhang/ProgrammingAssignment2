# Programming Assignment 2 - Lexical Scoping
# Stephanie Zhang
# R Programming 
# makeCacheMatrix creates a special "matrix", which is really a list containing a function to

# set the value of the matrix
# get the value of the matrix
# set the value of the matrix inverse
# get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse<- NULL
  set <- function(y) {
    x <<- y
    inverse<<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverse<<- inverse
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# The following function calculates the inverse of the special "vmatrix" 
# created with the above function. However, it first checks to see if the inverse matrix
# has already been calculated. If so, it gets the inverse matrix from the cache and 
# skips the computation. Otherwise, it calculates the inverse of the matrix and sets 
# the inverse of the matrix in the cache via the setInverse function.


cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
    
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
