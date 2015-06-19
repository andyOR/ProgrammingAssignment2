## Put comments here that give an overall description of what your
## functions do- Matrix inversion is usually a costly computation and there may be some benefit to 
##caching the inverse of a matrix rather than compute it repeatedly. The first function, 
#makeCacheMatrix creates  a special "matrix",which is a list containing a function to
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse of the matrix
##4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
      x <<- y
      inv <- NULL 
  }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse, 
         getinverse = getinverse)
}


## The following function returns the inverse of the matrix. It first checks if the inverse
## already been calculated. if yes, it gets the value and skip the computation. If
## not, it calculates the inverse, set the value  in the cache. 
cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if(!is.null(inv)) {
         message("getting cached data")
         return(inv)
  }
     data <- x$get()
     inv <- solve(data,...)
     x$setinverse(inv)
     inv
}
