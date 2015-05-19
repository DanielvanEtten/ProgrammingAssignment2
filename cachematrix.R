
#This programme contains two functions: makeCacheMatrix and cacheSolve


#how to use the two functions:
#1: first create a matrix:
# m <- matrix(rnorm(100,10), nrow=10, ncol=10)
#2: feed matrix to makeCacheMatrix function:
# cm <- makeCacheMatrix(m)
# 3 call cacheSolve
# cacheSolve(cm)



## makeCacheMatrix creates a list containing a function to:
# (1) set the value of the matrix
# (2) get the value of the matrix
# (3) set the value of the inverse
# (4) get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
  
 }



## CacheSolve takes a matrix and checks if the inverse of this matrix is stored in cache.
## if so it returns the solved Matrix from cache. If not it computes the matrix and then stores it in cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
 }


