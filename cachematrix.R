## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix initialize a specialized matrix object that allows caching of the matrix's inverse.
#It also returns a list of function setters and getters for the matrix and its inverse.
makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

#cacheSolve receives the matrix object returned by makeCacheMatrix as input and returns its cached inverse matrix.
#if the inverse matrix has not been saved, the function will calculate the inverse matrix and store it in cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}