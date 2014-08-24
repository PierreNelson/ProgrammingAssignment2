## A collection of functions to enable quick calculations of the inverse of 
## a matrix if the matrix inverse has already 

## The makeCacheMatrix function will encapsulate a matrix and provide the means
## to cache the inverse of a matrix.  It takes as parameter the matrix of interest
## and returns 4 supporting functions to store and retreive the value of the 
## matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  matrix <- NULL
  
  set <- function(y) {
    print ("resetting cache")
    matrix <<- y
    inv_matrix <<- NULL
  }
  
  get <- function() matrix
    
  setinv = function(inv) {
    inv_matrix <<- inv
  }
  
  getinv = function() inv_matrix

  matrix <- x
  list( set = set, get = get, setinv = setinv, getinv=getinv)
    
}

## cacheSolve will first determine if the matrix inverse has already been calculated
## and return the saved inverse.  If the matrix is changed, the cache is cleared

cacheSolve <- function(x, ...) {
       
  inv <- x$getinv()
  
  if (!is.null(inv)) {
    message ("getting cached data")
    return (inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  
  inv
       
}
