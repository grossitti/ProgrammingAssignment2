## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than computing it repeatedly.
## Below are two functions that are used to create a special object that stores a matrix and
## caches its inverse so that it can be quickly retrieved avoiding re-computation.



## This function creates a special "matrix" that is able to cache its inverse.
## It is called a special "matrix" because in reality it is a list containing four functions:
## 1. to set the value of the matrix; 2. to get the value of the matrix; 3. to set the value
## of the inverse, 4. to get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## However, it first checks to see if the inverse has already been calculated and cached.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, the function calculates the inverse of the matrix and stors the value in the
## cache using the setinverse function.
##
## The matrix needs to be square (invertible) for the function to work.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
  return (inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
