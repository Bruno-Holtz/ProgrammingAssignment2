## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  if(is.matrix(x) && nrow(x) == ncol(x)){
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(Y) inv <<- Y
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
  } else {
    message('input must be a square matrix')
    return()
  }
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  y <- x$getinv()
  if(!is.null(y)){
    message('Getting cache data')
    return(y)
  }
  pivot <- x$get()
  y <- solve(pivot, ...)
  x$setinv(y)
  y
}
