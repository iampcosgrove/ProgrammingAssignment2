## The functions enclosed are are intended to create a function that is capable 
## caching the inverse of a Matrix. 

## The makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse= getinverse)
}


## The cacheSolve function computes the inverse of the matrix and if previously 
## calculated will retrieve from the cache.

cacheSolve <- function(x, ...) {
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