# Unit Test
## Create a matrix named myMatrix

> myMatrix <- rbind(c(1, -1/4), c(-1/4, 1))


## Unit testing myMatrix

> myMatrix
      [,1]  [,2]
[1,]  1.00 -0.25
[2,] -0.25  1.00


## Create function makeCacheMatrix

> makeCacheMatrix <- function(x = matrix()) {
+     m <- NULL
+     set <- function(y) {
+         x <<- y
+         m <<- NULL
+     }
+     get <- function() x
+     setInverse <- function(solve) m <<- solve
+     getInverse <- function() m
+     list(set = set, get = get,
+          setInverse = setInverse,
+          getInverse = getInverse)
+ }

## Unit testing function makeCacheMatrix
## Run function makeCacheMatrix(myMatrix) and assign the value to cacheMatrix

> cacheMatrix <- makeCacheMatrix(myMatrix)

## Get the value of cacheMatrix

> cacheMatrix$get()
      [,1]  [,2]
[1,]  1.00 -0.25
[2,] -0.25  1.00

## Getting the inverse should give NULL value as we have not created the inverse of the matrix yet

> cacheMatrix$getInverse()
NULL

## Create function cacheSolve

> cacheSolve <- function(x, ...) {
+     m <- x$getInverse()
+     if(!is.null(m)) {
+         message("getting cached data")
+         return(m)
+     } else {
+         data <- x$get()
+         m <- solve(data)
+         x$setInverse(m)
+         return(m)
+     }
+ }

## Run cacheSolve on cacheMatrix

> cacheSolve(cacheMatrix)
          [,1]      [,2]
[1,] 1.0666667 0.2666667
[2,] 0.2666667 1.0666667

## Getting the inverse should give correct value

> cacheMatrix$getInverse()
          [,1]      [,2]
[1,] 1.0666667 0.2666667
[2,] 0.2666667 1.0666667

## Testing correctness of inverse (Result should identity matrix)

> cacheMatrix$get() %*% cacheMatrix$getInverse()
     [,1] [,2]
[1,]    1    0
[2,]    0    1

## Test the cacheing (If I run the cache solve again, the result should be returned from cache instead of running solve)

> cacheSolve(cacheMatrix)
getting cached data
          [,1]      [,2]
[1,] 1.0666667 0.2666667
[2,] 0.2666667 1.0666667

## Now lets change value of myMatrix

> myMatrix <- rbind(c(2/3, -1/4), c(-3/8, 1))
> myMatrix
           [,1]  [,2]
[1,]  0.6666667 -0.25
[2,] -0.3750000  1.00

## Run function makeCacheMatrix(myMatrix) and assign the value to cacheMatrix

> cacheMatrix <- makeCacheMatrix(myMatrix)

## Get the value of cacheMatrix

> cacheMatrix$get()
           [,1]  [,2]
[1,]  0.6666667 -0.25
[2,] -0.3750000  1.00

## Test the cacheing (Since the input had different matrix value, it is applying solve instead of fetching value from cache)

> cacheSolve(cacheMatrix)
          [,1]      [,2]
[1,] 1.7454545 0.4363636
[2,] 0.6545455 1.1636364

