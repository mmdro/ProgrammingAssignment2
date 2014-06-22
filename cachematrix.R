## A pair of functions that cache the inverse of a matrix. It is assumed that
## the supplied matrix is invertible.

## Function makeCacheMatrix creates a matrix object that can cache its inverse.
## It contains the following functions:
##    set
##    get
##    setinverse
##    getinverse

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # get the value of the matrix
    get <- function() {
        x
    }
    
    # set the value of the inverse
    setinverse <- function (inverse) {
        i <<- inverse
    }
    
    # get the value of the inverse
    getinverse <- function() {
        i
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function cacheSolve computes the inverse of the matrix returned
## by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve retrieves the inverse
## from the cache.

cacheSolve <- function(x, ...) {
    
    # retrieve the inverse
    i <- x$getinverse()
    
    # if the inverse has already been calculated, return it
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # if the inverse hasn't already been calculated:
    
    # get the value of the matrix
    data <- x$get()
    
    # calculate its inverse
    i <- solve(data)
    
    # set the value of the inverse in the cache
    x$setinverse(i)
    
    # return the inverse
    i
}

