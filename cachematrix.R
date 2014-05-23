### Assignment: Caching the Inverse of a Matrix

## Description: Matrix inversion is usually a costly computation 
##   and there may be some benefit to caching the inverse of a 
##   matrix rather than computing it repeatedly. This pair of 
##   functions cache the inverse of a matrix.


## This function creates a special "matrix" object
##   that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # when setting up an object, we set the object and a NULL cached version
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x  # get function returns the matrix object

  # set the inverse of the matrix if "setinverse" is called
  setinverse <- function(solve) m <<- solve
  
  # check for a solved version and then solve for an inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned 
##   by `makeCacheMatrix` above. If the inverse has already been 
##   calculated (and the matrix has not changed), then
##   `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # first check to see if there is an inverse already cached
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if the inverse has not been cached, first load the matrix object:
  data <- x$get()
  # then use the solve function based on the data object just created
  m <- solve(data, ...)
  # now set a cached version of the inverse
  x$setinverse(m)

  #and finally, return the resut:
  m
}
