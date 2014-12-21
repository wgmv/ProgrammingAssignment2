## There are two functions in this file: 
## 1 - Creates an environment that can cache the inverse of the matrix that is passed to the function
## 2 - Checks if the inverse had already been calculated and returns the cached value and caches it.
## The functions do not check if the inverse can be calculated

## This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  ##set <- function(y) {
  ##  x <<- y
  ##  Inv <<- NULL
  ##}
  
  get <- function() x
  setInv <- function(solved) Inv <<- solved
  getInv <- function() Inv
  list(get = get,  ## set = set, 
       setInv = setInv,
       getInv = getInv)

}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInv()
  if(!is.null(Inv)) {
    message("Getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInv(Inv)
  Inv
  
}
