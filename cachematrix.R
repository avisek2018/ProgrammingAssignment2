## These 2 functions can cache the inverse of a Matrix.

## makeCacheMatrix function creates a Matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## cacheSolve function computes the inverse of the Matrix returned by makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Retrieving Cached Data")
    return(inv)
  }
  myData <- x$get()
  inv <- solve(myData)
  x$setInverse(inv)
  inv

}
