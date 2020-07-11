##1.
## Creating a new matrix using get and set functions##
##makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse.

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
##2,
## Getting the inverse of a matrix from cache if already exists else finds inverse
##cacheSolve: This function computes the inverse of the special “matrix” from the makeCacheMatrix created above. If the inverse has already been calculated then the cachesolve will retrive it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- m$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data, ...)
  m$setinverse(inv)
  inv
}
