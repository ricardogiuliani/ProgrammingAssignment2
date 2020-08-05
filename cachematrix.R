## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse matrix
  i <- NULL
  
  ## Method to set the matrix
  set <- function(matrix) {
    x <<- matrix
    i <<- NULL
  }
  
  ## Method to get the matrix
  get <- function() x
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) i <<- inverse
  
  ## Method to get the inverse of the matrix
  getInverse <- function() i
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Checks if the inverse has already been calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix data
  data <- x$get()
  
  ## Calculate the inverse 
  m <- solve(data)
  
  ## Set the inverse 
  x$setInverse(m)
  
  ## Return the matrix
  m
}
