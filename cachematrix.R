## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##cache the inverse of a matrix
##benefit of matrix inversion
##instead of repeat computer
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y){
  x <<- y
  inv <<- NULL
}
get <- function()x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function()inv
list(set = set,
     get = get,
    setInverse = setInverse,
    getInverse = getInverse)

## Write a short comment describing this function
##function that creates a special matrix that can cache its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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


## Write a short comment describing this function
##if the inverse was calculated and the matrix was not changed
## with cacheSolve() get the inverse of the “matrix” 
