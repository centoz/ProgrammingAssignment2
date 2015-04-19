## Put comments here that give an overall description of what your
## functions do

## Creates a matrix that can be used to cache its inverse
##  when calling the cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
  # Clean variable used to store inverse
  inv <- NULL
  
  # Sets the matrix from parameter and reinitialise 
  #  the inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Returns matrix
  get <- function() {
    x  
  }
  
  # Sets a matrix specified as the inverse
  setinverse <- function(matrix) {
    inv <<- matrix
  } 
  
  # Returns the inverse of the matrix if stored
  getinverse <- function() {
    inv
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculates the inverse of a matrix and caches result
##  to allow reuse without recomputing inverse
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  # Check for existing inverse - use cached inverse if known
  if(!is.null(inv)) {
    message("Inverse retrieved from cache")
    return(inv)
  }
  
  # Retrieve, solve for inverse of matrix and set inverse
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinverse(inv)
  inv
}
