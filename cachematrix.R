## Example of how to use lexical scoping to cache the inverse of a matrixin the
## local environment of a function call

## This function returns a list of functions to access and manipulate a matrix, x
## functions:
## *set: sets the value of the matrix, also resets(nulls) value of s, its inverse
## *get: returns the value of the matrix, x
## *setsolve: sets the value of s, the inverse of x
##            the companion function 'cacheSolves' computes s when called
## *getsolve: returns the value of s, the inverse of x
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## computes the value of s, the inverse of x
## *the matrix x refers to the a local variable stored in the environment of the
##  function makeCacheMatrix.
## *The x in the argument list of this function refers to the function list returned
##  by makeCacheMatrix, which is stored in the global environment
## *first checks if value of s is already cached (i.e. not NULL) in the environment
##  of x. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
