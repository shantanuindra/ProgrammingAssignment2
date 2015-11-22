
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL 
  ## set function changes the matrix stored in the main function 
 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## get function returns the matrix x stored in the main function 
  get <- function()x
  setInverse <- function(inverse) i<<-inverse
  getInverse <- function()i
  ## list is used to assign makeCacheMatrix to an object, which contains 4 functions 
    list(set = set, get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}

cacheSolve <- function(x, ...){
  i <- x$getInverse()
  ## if condition verifies the value i exists and is not NULL, if it was stored previously using getInverse
  ## if it exists, then it returns the message and the cached value 
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  ## if it is not cached, then data gets the matrix stored with makeCacheMatrix, 
  ## i calculates the inverse of the matrix using solve function, 
  ## and x$setInverse(i) stores it in the object generated, assigned with makeCacheMatrix
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
  }
