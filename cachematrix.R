## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix create a special vector with four defined functions: get, set,
## getinverse and setinverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverseParam) inverse <<- inverseParam
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## this function return the inverse of a matrix defined previously in a makeCacheMatrix
## if inverse was calculated, only return the value stored in inverse variable
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    matrix <- x$get()
    if (diff(dim(matrix)) != 0){
      message("can't calculate the inverse")
      return("error")
    }
    inverse <- solve(matrix, ...)
    x$setinverse(inverse)
    inverse
}
