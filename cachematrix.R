## Put comments here that give an overall description of what your
## functions do

## This function creates a list of special functions which are used
## to create an inverse of a matrix and to cache the results if the matrix
## is unchanged to avoid unnecessary calculations
## set function assigns the input data to a global variable and sets
## the inverse to NULL
## get function returns the matrix 
## setinverse solves the matrix inversion
## getinverse returns the latest inverse calculation (which may be NULL)

makeCacheMatrix <- function(x = matrix(), ...) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
  }
  

## cacheSolve checks the result of getinverse and if it's not null, then
## returns the cached value, otherwise does the inverse calculation and sets it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
