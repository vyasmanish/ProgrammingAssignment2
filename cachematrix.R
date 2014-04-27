## makeCacheMatrix function helps caching of inverse of the input matrix
## so that whenever cacheSolve is called and the input matrix is same as
## prevoiusly inputed matrix, the result is retured from the cace instead
## of applying solve function to the input

## Make sure we run makeCacheMatrix before cacheSolve

## This function caches the inverse of the input matrix as well as returns
## list of functions set, get, setInverse and getInverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function fetches inverse from cache, if the cache is not null it returns inverse
## from cache else applies solve function to input matrix

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    } else {
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        return(m)
    }
}