## create the matrix object by 'makeCacheMatrix' and cache the inverse matrix by 'cachSolve'

## to create special matrix (global 'x' for matrix variable, global 'inverseX' for inverse matrix)

makeCacheMatrix <- function(x = matrix()) {
      
      inverseX <- NULL
      set <- function(y){
            x <<- y
            inverseX <<- NULL
      }
      get <- function() x
      setInverse <- function(inverseMatrix) inverseX <<- inverseMatrix
      getInverse <- function() inverseX
      list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## to retrive the inverse matrix from global 'inverseX' of x, if get null value then calculate 
## and inverse matrix to global 'inverseX'. Once 'inverseX' is calculated, it will not be
## calculated repeatedly.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      inverseX <- x$getInverse()
      if(!is.null(inverseX)){
            message("getting cached data")
            return(inverseX)
      }
      data <- x$get()
      inverseX <- solve(data,...)
      x$setInverse(inverseX)
      inverseX
}
