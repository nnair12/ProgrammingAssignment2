
## This function creates and returns a list of functions that can be
## used to set an inverted matrix in the cache or to get an inverted
## matrix from the cache.
makeCacheMatrix <- function(x = matrix()) 
{
    cacheMatrix <- NULL # for cached value, initialized as null
    
    ## to create a matrix in the current environment
    set <- function (y)
    {
      x <<- y
      cacheMatrix <<- NULL
    }
    
    ## to obtain the original matrix
    get <- function() {
      x
    }
    
    ## Inverts the matrix and stores it in the cache
    setInverse <- function (solvedMatrix) {
      cacheMatrix <<- solvedMatrix
    }
    
    ## Obtains the inverted matrix from the cache
    getInverse <- function() {
      cacheMatrix
    }
    
    ## Returns the list of created functions
    list (set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computers the inverse of the special matrix returned
## by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then cachesolve should retrieve
## the inverse from the cache.
cacheSolve <- function(x, ...) 
{
    inverse <- x$getInverse()
    
    ## Runs if the inverse has already been calculated
    ## (and the matrix hasn't been changed).
    if (!is.null(inverse)) 
    {
        message ("Getting cached data")
        return (inverse)
    }
    
    ## If inverse wasn't calculated or matrix was changed
    newMatrix <- x$get()
    inverse <- solve (newMatrix, ...)
    x$setInverse(inverse)
    inverse
}
