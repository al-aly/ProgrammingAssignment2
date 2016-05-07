## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
# First function makeCacheMatrix creates a special list containing functions to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(z){
         x <<- z
         inv <<- NULL
  }
  get <- function()x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  
}


## Write a short comment describing this function
## The function calculates the inverse matrix from makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <-x$get()
  inv <-solve(data,...)
  x$setInverse(inv)
  inv
  
  }
