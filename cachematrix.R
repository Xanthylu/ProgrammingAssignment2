## Caching the inverse of a matrix 
## In two functions

## This function creates a special matrix called makeCacheMatrix that will be able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      invM <- NULL
      set <- function(y) {
            x <<- y
            invM <<- NULL
      }
      get <- function() x
      setInverse <- function(solveMatrix) invM <<- solveMatrix
      getInverse <- function() invM
      list(set = set, get = get, 
            setInverse = setInverse, 
            getInverse = getInverse)
}


## This functions takes the special "matrix" (makeCacheMatrix) and computes the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      invM <- x$getInverse()
      if(!is.null(invM)){
            message("getting cached data")
            return(invM)
      }
      data <- x$get()
      invM <- solve(data)
      x$setInverse(invM)
      invM      
}
