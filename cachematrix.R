## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates and returns a list of functions. It is a Global function
makeCacheMatrix <- function(x = matrix()) {
# stores the inv value and initialize to NULL 
  inv <- NULL
# creates the matrix in the working environment  
  set <- function(y) {
# assign a value to an object
    x <<- y
    inv <<- NULL
  }
# get the value of matrix
  get <- function() x
# invert the matrix and stored in inv
  setInverse <- function(inverse) inv <<- inverse
# get the inverted matrix from inv
  getInverse <- function() inv
# return the created functions to the working environment
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function

## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
cacheSolve <- function(x, ...) {
## to get the inverse of a matrix stored in inv
  inv <- x$getInverse() 
# return inverted matrix from inv if it exists 
  if (!is.null(inv)) {
    message("getting cached data")
# display the matrix in console
    return(inv)
  }
# create matrix in working environment since it does not exist
  mat <- x$get()
# set and return inverse of matrix
  inv <- solve(mat, ...)
# set inverted matrix in inv
  x$setInverse(inv)
#print the value of inv
  inv
}
