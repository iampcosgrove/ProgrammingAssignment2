## The functions enclosed are are intended to create a function that is capable 
## caching the inverse of a Matrix. 

## The makeCacheMatrix function creates a special "matrix" object that can 
##    cache its inverse.
## Example of functions required to execute:
## #1- Run cachematrix.R file to execute makeCacheMatrix & cacheSolve functions.
## #2- Establish matrix in environment: mymatrix <- matrix(1:4, nrow = 2, ncol =2)
## #3- Execute cache of matrix into environment: my_cache <- makeCacheMatrix(mymatrix)
## #4- Execute inverse of matrix (if repeated twice will pull inverse of data from cache: 
##       cacheSolve(mymatrix)

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