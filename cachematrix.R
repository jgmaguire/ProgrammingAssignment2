## Put comments here that give an overall description of what your functions do
# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than computing it repeatedly (there 
# are also alternatives to matrix inversion that we will not discuss here). Your 
# assignment is to write a pair of functions that cache the inverse of a matrix.

## Write a short comment describing this function
# Function: makeCacheMatrix
# Parameter: x (matrix) 
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  getMatrix <- function() {
    x
  }
  
  setInverse <- function(i) {
    inverse <<- i
  }
  
  getInverse <- function() {
    inverse
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)    
}


## Write a short comment describing this function
# Function: cacheSolve
# Parameter: x (matrix) 
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    return(inverse)
  }

  cacheMatrix <- x$getMatrix()
  inverse <- solve(cacheMatrix)
  x$setInverse(inverse)
  
  return(inverse)
}
