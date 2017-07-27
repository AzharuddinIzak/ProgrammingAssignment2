## To calculate Inverse of a square Matrix takes time, so it is better to 
## cache the inverse of any given matrix rather than calculating afresh at each instance.

## below function creates a special matrix with a list containing functions
## to set value and get value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
## lets initiate our cache with NULL
  cache <- NULL
  
  ## Creating the matrix in the working environment
  set <-function(y = matrix()) {
    x <<- y
    ## <<- enables to assign a value to an object in a different environment than the current one
    cache <<- NULL
  }
 
  get<-function() x
  
  setInverse <- function(inverse) cache <<- inverse
 
  getInverse <- function() cache
  
  ## now returning the above functions as a list in the working environment
  ## to be used as input to cachesolve function
  list(set = set , get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Now that we have created the necessary functions we need to use them appropriately
## We check if the inverted matrix exist in cache if true we do not compute again
## we show the cached value, if not we need to calculate and display

cacheSolve <- function(x=matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
  cache <- x$getInverse()
  ## we need to check with if this is null or has the already calculated cached matrix
  if(!is.null(cache)){
    return(cache)
  }
  ##if it does not exist in cache we need to create it
  matrix <- x$get()
  cache <- solve(matrix, ...)
 ## we also need to set this value to cache, to avoid recalculating it again in future.
  x$setInverse(cache)
  ## lets display and check.
  return(cache)
}
