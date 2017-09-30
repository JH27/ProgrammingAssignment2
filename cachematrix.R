

## Below is the function to set the variables.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {                          ## Set the initial value
        x <<- y
        i <<- NULL
    }
    get <- function() x  ## get the cached value
    setinverse <- function(inverse) i <<- inverse ## set the inverse of the matrix
    getinverse <- function() i
    list(set = set, get = get,                    ## Return the list of all values
         setinverse = setinverse,
         getinverse = getinverse)
}


## Below function is to create an inverse matrix with cached values

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {                            ## If clause to get the cached data if it exists
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
