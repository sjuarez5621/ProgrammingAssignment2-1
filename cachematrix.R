## The following function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()){
  ## Initiate inverse property
  inv <- NULL
  ## Set the matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  ## Get the matrix
  get <- function() {x}
  ## Set the inverse of a matrix
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...){
    inv <- x$getInverse()
  ## Return a matrix that is the inverse of 'x'
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  ## Return the matrix
  inv
}
