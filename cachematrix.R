## Put comments here that give an overall description of what your
## functions do

## The following function will take a matrix as input and calculate the
## inverse of this function.  The function will save both of these values.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function will take a matrix as input, check to see if the
## matrix has already been solved via the makeCacheMatrix solution. If the
## matrix has previously been solved the data is retrieved, if it has not
## already been solved the function will compute the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("retriving cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
