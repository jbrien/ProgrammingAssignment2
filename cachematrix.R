## Example
##
## > matrix <- makeCacheMatrix(matrix(c(1:4),nrow=2,ncol=2))
## > cacheSolve(x)
##         [,1] [,2]
##   [1,]   -2  1.5
##   [2,]    1 -0.5

## Creates the functions for the Object
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inv <<- i
  getInverse <- function() inv 
  list(get = get, set = set,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Flips the matrix unless it has been computed prior
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
