## makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse. 
## cacheSolve: This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache. 
## The first function, makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- solve
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
  if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
