## As indicated in the example and in the assigment this function seeks to inverse a given matrix.
## This function creates a matrix that can be cached. I set the values on vector to matrix and then defined the given values to set the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
  set <- function(y){ 
    x <<- y 
    inv <<- NULL
  }
  get <- function() x 
  setInverse <- function(inverse) inv <<- inverse 
  getInverse <-function() inv 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## the function 'cacheSolve' takes the given matrix and returns the inverse of it. 

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
    if(!is.null(inv)){
      message("getting cached inverted matrix")
      return(inv)
    }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
