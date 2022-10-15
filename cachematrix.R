## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Caching the Inverse of a Matrix:
## Matrix inversion is a costly computation, however there may be some 
## benefit to caching the inverse of a matrix instead of compute it repeatedly.
## here is a pair of functions that are used to create a special object which 
## stores a matrix and caches its inverse.

## this function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function
## the function below computes the inverse of the special "matrix" created by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed),  it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
        ## Return a matrix that is the inverse of 'x'

