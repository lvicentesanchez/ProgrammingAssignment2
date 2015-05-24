## These pair of functions allow to create and invert matrixes; as inverting big matrixes could be a costly process
## it caches the value of the inverse to re-use it if the contents of the matrix are not changed.

## Creates a cacheable matrix with the following methods:
## 
##   - set(); method that set the value of the matrix and clears the cached inverse
##   - get(); method that get the value of the matrix
##   - setinv(); method that set the inverse of the matrix
##   - getinv(); method that get the inverse of the matrix 

makeCacheMatrix <- function(m = matrix()) {
  inverse <- NULL
  set <- function(x) {
    m <<- x
    inverse <<- NULL
  }
  get <- function() m
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Computes the inverse of a cacheable matrix. If the inverse has already been calculated, it returns the cached value.
## If the inverse hasn't been calculated yet, it calculates and caches the inverse before returning it.

cacheSolve <- function(m, ...) {
  inverse <- m$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- m$get()
  inverse <- solve(data, ...)
  m$setinv(inverse)
  inverse
}
