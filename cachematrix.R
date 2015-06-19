## Put comments here that give an overall description of what your
## functions do- the first function creates  a special "vector"

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
      x <<- y
      m <- NULL 
  }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse, 
         getinverse = getinverse)
}


## Write a short comment describing this function- The following function calculates
## the inverse of the special "vector" created with the above function. 
cacheSolve <- function(x, ...) {
     m <- x$getinverse()
     if(!is.null(m)) {
         message("getting cached data")
         return(m)
  }
     data <- x$get()
     m <- solve(data,...)
     x$setinverse(m)
     m
}
