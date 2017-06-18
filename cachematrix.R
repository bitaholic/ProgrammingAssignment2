## coursera week3 assignment

## make cachematrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## If x(CacheMatrix) have cached result, return it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("return cashed value[inverse]")
    return(inverse)
  } else {
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
  }
}