##  Functions to show how to caching the inverse of a matrix and speed up the
## inverse operation by retriving the inverse matrix from the cache if it 
## exits. It is needed to define a special matrix (makeCacheMatrix) and a 
## new Solve function (cacheSolve) to handle the inverse of that special
## matrix

## This first function defines a new matrix starting from a matrix argument 
## and outputs a list with 4 methods to access and set the matrix and its 
## inverse matrix 
makeCacheMatrix <- function(x = matrix()) {
    ix  <- NULL
    set <- function(y) {
        x <<- y
        ix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) ix <<- inverse 
    getinverse <- function() ix
    #return all this as a list   
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'. 
## Checks first if the inverse matrix is already available in the cache 
cacheSolve <- function(x, ...) {
      # Inverse matrix in the cachw? 
      ix<-x$getinverse()
      if(!is.null(ix)) {
          message("getting cached data")
          return(ix)
      }
      # Calculates the inverse matrix because it was not available 
      xdata <- x$get()
      ix <- solve(xdata, ...)
      x$setinverse(ix)
      ix
}
