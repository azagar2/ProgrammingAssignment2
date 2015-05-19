## These two functions create a special matrix object that can be modified 
## (through getters and setters) and inverted

## This function makes a special matrix object 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  ## Special matrix object
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of a square matrix that is 
## encapsulated in a special matrix object 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("Getting cached matrix")
    return(inverse)
  }
  ## If inverse has not yet been cached, do the following
  message("Caching inverse of matrix")
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
