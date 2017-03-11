## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## In this file two functions are defined to cache the matrix inversion.

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to

## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## By default N (the inverrse matrix is set to null)
  m <- NULL
  ## Cache the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Retrive the matrix
  get <- function() x
  ## cache the inverse matrix
  setinverse <- function(solve) m <<- solve
  ## Retrive the inverse
  getinverse <- function() m
  ## List with all the functions needed for inversing and caching
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    # check to see if the inverse matrix is already cached
    message("getting cached data")
    return(m)
  }
  ## Compute the inverse matrix and strore it in the cache
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
