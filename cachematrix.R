
## There are some functions for creating a special object: makeCacheMatrix creates a "matrix" object that can cache its inverse.

## Lets "a" means "inverse"


makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set <- function(y) {
                x <<- y
                a <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) a <<- inverse
        getInverse <- function() a
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## And the second function cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix.  If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        a <- x$getInverse()
        if (!is.null(a)) {
          message("getting cached data")
          return(a)
        }
        m <- x$get()
        a <- solve(m, ...)
        x$setInverse(a)
        a
}
