## The makeCacheMatrix function calculate and store inverse
## of matrix. It will only be called when cacheSolve fails to
## find an existing result. If the cacheSolve does find a result,
## the function will retrieve the result from cache.

## This function defines sub-functions and calculates inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function calls the makeCacheMatrix function and
## checks if the result already exist in cache. If it does, retrieve
## the inverse from cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
    ## Return a matrix that is the inverse of 'x'
}
