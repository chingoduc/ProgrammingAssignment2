## These functions will allow to create the inverse of an invertible matrix, store it as a cached
## in an environment and at the next access to the the cached version, the cached will be returned.

## makeCacheMatrix with a set of functions namely "set", "get", "setInverse", "getInverse", 
## creates a new vesrion of a matrix together with its cached Inverse matrix.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function returns the Inverse matrix of a matrix. If this is the first time, a cached will be created, 
## stored for later accesses before returning the Inverse. For later access, 
## the cached copy will be returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached Inverse")
    return(inv)
  }
  mdata <- x$get()
  inv <- solve(mdata)
  x$setInverse(inv)
  inv
}
