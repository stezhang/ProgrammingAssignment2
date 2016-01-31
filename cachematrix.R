# Programming Assignment 2 - Lexical Scoping
# Stephanie Zhang
# R Programming 
# makeCacheMatrix creates a special "matrix", which is really a list containing a function to

# set the value of the matrix
# get the value of the matrix
# set the value of the matrix inverse
# get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  iv<- NULL
  set <- function(y) {
    x <<- y
    iv<<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) iv<<- inverse
  getInverse <- function() iv
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
  iv <- x$getInverse()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
    
  }
  data <- x$get()
  in <- solve(data, ...)
  x$setInverse(iv)
  iv
}
