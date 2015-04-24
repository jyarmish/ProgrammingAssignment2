## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## computing it repeatedly. The following pair of functions cache 
## the inverse of a matrix so that it need not be repeatedly 
## computed after being done once.

## The first function, `makeCacheMatrix` creates a special "matrix"
## object, that can cache its inverse. 
## (In effect, it is a list containing functions that
##      1.  set the values of the matrix
##      2.  get the values of the matrix
##      3.  sets the values of the inverse
##      4.  get the values of the inverse
## )

makeCacheMatrix <- function(x,nrow=sqrt(length(A)),ncol=nrow,byrow=FALSE) {
  m = matrix(x,nrow,ncol,byrow)
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This second function, 'cacheSolve' computes the inverse of the 
## special "matrix" object returned by 'makeCacheMatrix' above. 
## However, it first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips 
##the computation.

cacheSolve <- function(m, ...) {
  i <- m$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- m$get()
  i <- solve(data, ...)
  m$setinverse(i)
  i
}