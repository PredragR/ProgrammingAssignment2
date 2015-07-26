## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  
  setInverse <- function(x) inverse <<- solve(x)

  getInverse <- function() inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  i <- x$getInverse()
  
  if(!is.null(i)) {
    message("getting cached")
    return(i)
  }
  original <- x$get()
  i <- solve(original)
  x$setInverse(i)
  i

}
