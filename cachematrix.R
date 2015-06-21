## Modified by: Daniel Kunimoto

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinverse <- function(y) inv <<- y 
     getinverse <- function() inv
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getinverse()
     if(!is.null(inv)){
          print("getting cached data")  # Retrieves the inverse already cached
          inv                           # in memory
     }
     else
     {
          data <- x$get()
          inv <- solve(data)            # Calculates the inverse and sets it
          x$setinverse(inv)             # in memory when not already assigned
     }
}
