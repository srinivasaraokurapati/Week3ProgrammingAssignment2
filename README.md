# Week3ProgrammingAssignment2
# makeCacheMatrix will create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
  x <<- y
  inv <<- NULL
}
get<-function() x
setinverse<-function(inverse) inv <<- inverse
getinverse<-function() inv
list(set = set, get = get,
   setinverse = setinverse,
   getinverse = getinverse)
}

## cacheSolve calculates the inverse of the matrix created by makeCacheMatrix.
## if the inverse has already been calculated,
## the inverse will be taken from the cache instead.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}
